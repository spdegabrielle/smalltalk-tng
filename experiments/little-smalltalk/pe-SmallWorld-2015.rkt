#lang racket/gui
;; Loader for images (version 1 format) from Russell Allen's 2015
;; variant of SmallWorld, a Tim Budd-authored Little Smalltalk
;; descendant.

(require racket/bytes)
(require (only-in sha bytes->hex-string))
(require "object-memory.rkt")
(require "primitives.rkt")

(define-logger vm)
(define-logger vm/jit)
(define-logger vm/jit/code)
(define-logger vm/jit/recompile)
(define-logger vm/jit/recompile/candidates)

;; Runtime support: We use `eval` with namespace `ns` to allow
;; generated code to access bindings in this module.
(define-namespace-anchor ns-anchor)
(define ns (namespace-anchor->namespace ns-anchor))

;;===========================================================================
;; Structures

(struct pe-VM VM (cache image-filename)
  #:methods gen:vm-callback
  [(define (vm-block-callback vm action)
     ;; Runs action in a new thread
     (lambda args
       (thread (match action
                 [(unffiv block-proc)
                  (lambda () (apply block-proc (outermost-k vm) args))]
                 [_
                  (block->thunk vm action args)]))))])

;; Just as the plain interpreter, `run-SmallWorld-2015.rkt`, builds
;; contexts at runtime describing a method activation, the JIT builds
;; contexts at compile time describing a method activation.
;;
;; Each context includes accumulator registers shared among all
;; contexts inlined into the top-level method being compiled, as well
;; as registers particular to itself.
;;
;; Runtime contexts include these registers:
;;   - method, the bytecoded method being interpreted
;;   - arguments, an Array of arguments to this activation
;;   - temporaries, an Array of temporaries for this activation
;;   - stack, an Array of size (slotAt method 3), an empty ascending stack
;;   - ip, an index into `method`'s bytecode
;;   - stack-top, an index into `stack`
;;   - previous-ctx, either nil or a reference to a calling context
;;
;; Our contexts will include compile-time analogues of these. Almost
;; everywhere that a runtime context refers to a value, our
;; compile-time contexts will refer to an abstract value instead.
;;
;; Each context includes:
;;   - vm, the compile-time vm
;;   - method, a concrete value
;;   - arguments, a Racket vector of abstract-values
;;   - temporaries, a symbol naming the Racket-level temporaries vector
;;   - stack, a Racket list of abstract-values; car = top of stack
;;   - ip, a Racket number
;;   - labels, a hashtable of code fragments roughly corresponding to basic blocks
;;   - previous, the next context in the chain
;;   - home, #f for non-blocks, otherwise the home context of a block
;;   - state, accumulator registers
;;
;; The accumulator registers are:
;;   - litmap, a Racket mutable hash table mapping actual runtime
;;     values to compile-time variable names (symbols)
;;   - pic-list-rev, a Racket list of symbols naming PICs in the
;;     compiled method
;;   - old-picmap, either #f or a hash indexing PICs from a previous
;;     compilation, for dynamic type feedback
;;   - histories, a Racket parameter holding a list of lists of
;;     `definition` structures
;;
(struct DynamicCtx (var) #:transparent)
(struct Ctx (vm method arguments temporaries stack ip labels previous home state) #:transparent
  #:methods gen:custom-write
  [(define (write-proc c port mode)
     (fprintf port "#<~a>" (format-Ctx c)))])

(struct State (litmap [pic-list-rev #:mutable] old-picmap histories) #:transparent)

(struct compiled-method-info (bytecode-method pics stable?))

(struct cached-method (class name-bytes [bytecode-method #:mutable] [proc #:mutable]))

(struct definition (var purity absval) #:transparent)

(struct AbsVal (expr desc) #:transparent)

(struct Unknown ())
(struct Constant (value) #:transparent)
(struct Obj (class slots) #:transparent)
(struct Bv Obj (bytes) #:transparent)
(struct Ffiv Obj (value) #:transparent)

;;===========================================================================
;; Polymorphic Inline Caches - PICs

(define pic-reserved 0)
(define pic-entry-count 3)

(define (pic) ;; pic-entry-count ×3 - class, method, and count.
  (vector #f #f 0
          #f #f 0
          #f #f 0))
(define (extended-pic c0 m0 c1 m1 c2 m2) ;; normal pic plus previous knowledge
  (vector #f #f 0 #f #f 0 #f #f 0
          c0 m0 0 c1 m1 0 c2 m2 0))

(define (pic-size pic) (quotient (- (vector-length pic) pic-reserved) pic-entry-count))
(define (pic@ pic index offset) (vector-ref pic (+ pic-reserved offset (* index 3))))
(define (pic@! pic index offset v) (vector-set! pic (+ pic-reserved offset (* index 3)) v))

(define (pic-bump! pic index)
  (define o (+ pic-reserved 2 (* index 3)))
  (vector-set! pic o (+ 1 (vector-ref pic o))))

(define empty-pic-extension (for/list [(i (in-range pic-entry-count))] '(#f #f)))

;;===========================================================================
;; Dynamic Deoptimization

(define (build-jit-context vm previous-context args method ip temporaries stack)
  ;; TODO: build block contexts instead of just pretending everything is a method...
  (define max-stack (slotAt method 3))
  (mkobj (VM-Context vm)
         method
         (obj (VM-Array vm) args)
         (obj (VM-Array vm) temporaries)
         (obj (VM-Array vm) (vector-append stack (make-vector (- max-stack (vector-length stack))
                                                              (VM-nil vm))))
         ip
         (vector-length stack)
         previous-context))

;;===========================================================================
;; Method cache; relationship between bytecoded and compiled methods

(define (lookup-method/cache vm class name-bytes)
  (define class-cache (hash-ref! (pe-VM-cache vm) class make-weak-hash))
  (hash-ref! class-cache
             name-bytes
             (lambda () (cached-method class name-bytes #f #f))))

(define (bytecode->cached-compiled vm class method)
  (lookup-method/cache vm class (bv-bytes (slotAt method 0))))

(define (compiled->bytecode cmethod)
  (compiled-method-info-bytecode-method (cmethod)))

(define (unwrap-cached-method vm cm)
  (or (cached-method-proc cm)
      (match cm
        [(cached-method class name-bytes _bcm _proc)
         (define bcm (lookup-method vm class name-bytes))
         (define proc (and bcm (compile-method-proc vm class bcm #f)))
         (set-cached-method-bytecode-method! cm bcm)
         (set-cached-method-proc! cm proc)
         proc])))

(define (invalidate-cached-method! cm)
  (set-cached-method-bytecode-method! cm #f)
  (set-cached-method-proc! cm #f))

;;===========================================================================
;; Runtime method lookup via PIC

(define (lookup-message/jit vm pic class selector)
  (let search-pic ((slot-index 0))
    (define this-class (pic@ pic slot-index 0))
    (if (eq? this-class class)
        (begin (pic-bump! pic slot-index)
               (or (unwrap-cached-method vm (pic@ pic slot-index 1))
                   (send-dnu vm class selector)))
        (let* ((next-slot-index (+ slot-index 1))
               (more-slots-to-check? (and this-class (< next-slot-index pic-entry-count))))
          (if more-slots-to-check?
              (search-pic next-slot-index)
              (let* ((cm (lookup-method/cache vm class (bv-bytes selector))))
                (when (not this-class)
                  (pic@! pic slot-index 0 class)
                  (pic@! pic slot-index 1 cm)
                  (pic@! pic slot-index 2 1))
                (or (unwrap-cached-method vm cm)
                    (send-dnu vm class selector))))))))

(define ((send-dnu vm class selector) ctx . args)
  (define arguments (obj (VM-Array vm) (list->vector args)))
  (define dnu-name-bytes #"doesNotUnderstand:")
  (match (unwrap-cached-method vm (lookup-method/cache vm class dnu-name-bytes))
    [#f (error 'send-message* "Unhandled selector ~a at class ~a" selector class)]
    [dnu-method
     (log-vm-warning "DNU -- arguments ~a class ~a selector ~a" arguments class selector)
     (dnu-method ctx (slotAt arguments 0) (mkobj (VM-Array vm) selector arguments))]))

;;===========================================================================
;; Compilation State

(define (top-compilation vm receiver-class method old-picmap top-k)
  (define litmap (make-hasheq))
  (Ctx-log 'top-compilation
           (Ctx vm
                method
                (for/vector [(i (selector-string-arity (method-name method)))]
                  (if (zero? i)
                      (AbsVal 'self (Obj (gen-lit litmap receiver-class) #f))
                      (AbsVal (mksym "arg~a" (- i 1)) (Unknown))))
                (gensym 'temps)
                '()
                0
                (make-hash)
                (DynamicCtx top-k)
                #f
                (State litmap
                       '()
                       old-picmap
                       (make-parameter '())))))

(define (selector-string-arity str)
  (define colon-count (for/sum [(c str)] (if (eqv? c #\:) 1 0)))
  (cond [(positive? colon-count) (+ colon-count 1)]
        [(char-alphabetic? (string-ref str 0)) 1]
        [else 2])) ;; assume binary operator

(define (mksym fmt . args) (string->symbol (apply format fmt args)))

(define (Ctx-log who c)
  (log-vm/jit/code-debug "~a ~a ~adefined in ~v (depth ~a)"
                         who
                         (Ctx-name c)
                         (if (Ctx-home c) "(BLOCK) " "")
                         (slotAt (Ctx-method c) 5)
                         (Ctx-depth c))
  (log-vm/jit/code-debug "  bytecode: ~a\n----\n~a\n----"
                         (bytes->hex-string (bv-bytes (slotAt (Ctx-method c) 1)))
                         (bv->string (slotAt (Ctx-method c) 6)))
  c)

(define (inline-compilation vm method actual-avs temporaries ip previous home state)
  (Ctx-log 'inline-compilation
           (Ctx vm
                method
                actual-avs
                (or temporaries (gensym (format "temps~a" (method-name method))))
                '()
                ip
                (make-hash)
                previous
                home
                state)))

(define (Ctx-depth c)
  (if (DynamicCtx? c)
      0
      (+ 1 (Ctx-depth (Ctx-previous c)))))

(define (Ctx-arg c n)
  (vector-ref (Ctx-arguments c) n))

(define (Ctx-receiver c)
  (Ctx-arg c 0))

(define (Ctx-receiver-class c)
  (Constant-value (AbsVal-desc (ObjClass (Ctx-vm c) (Ctx-receiver c)))))

(define (Ctx-name c)
  (method-name (Ctx-method c) (Ctx-receiver-class c)))

(define (already-compiling? c class method)
  (let check ((c c))
    (cond [(DynamicCtx? c) #f]
          [(and (eq? (Ctx-receiver-class c) class) (eq? (Ctx-method c) method)) #t]
          [else (check (Ctx-previous c))])))

(define (gen-lit* litmap lit)
  (if (number? lit)
      lit
      (hash-ref! litmap lit (lambda ()
                              (define n (hash-count litmap))
                              (cond
                                [(bv? lit) (mksym "lit~a-~a" n (bv->string lit))]
                                [(list? lit) (mksym "lit~a" n)]
                                [(vector? lit) (mksym "pic~a" n)]
                                [else (mksym "lit~a-~a" n lit)])))))

(define (gen-lit litmap lit)
  (AbsVal (gen-lit* litmap lit) (Constant lit)))

(define (Ctx-litmap c)
  (State-litmap (Ctx-state c)))

(define (Ctx-lit c literal)
  (gen-lit (Ctx-litmap c) literal))

(define (Ctx-update c new-ip stack-transformer)
  (struct-copy Ctx c [ip new-ip] [stack (stack-transformer (Ctx-stack c))]))

(define (Ctx-push c v)
  (Ctx-update c (Ctx-ip c) (lambda (s) (cons v s))))

(define (Ctx-drop c n)
  (Ctx-update c (Ctx-ip c) (lambda (s) (drop s n))))

(define (Ctx-goto c ip)
  (Ctx-update c ip values))

(define (Ctx-push-and-goto c ip v)
  (Ctx-update c ip (lambda (s) (cons v s))))

(define (format-Ctx c)
  (string-join (reverse
                (let pieces ((c c))
                  (if (DynamicCtx? c)
                      '()
                      (cons (format "~a @~a" (Ctx-name c) (Ctx-ip c))
                            (pieces (Ctx-previous c))))))
               ","
               #:before-first "["
               #:after-last "]"))

;;===========================================================================
;; Compilation and code generation

(define (compile-method-proc compile-time-vm class method old-picmap)
  (define top-k (gensym 'top-k))
  (define c (top-compilation compile-time-vm class method old-picmap top-k))
  (define body-code (gen-code c)) ;; imperative!
  (define pic-infos (reverse (State-pic-list-rev (Ctx-state c))))
  (define pic-infos-exp (gen-lit* (Ctx-litmap c) pic-infos))
  (define stable? (equal? (if old-picmap (list->set (hash-keys old-picmap)) 'unknown)
                          (list->set (map car pic-infos))))
  (log-vm/jit/recompile-debug "Evaluating stability of ~a:" (Ctx-name c))
  (log-vm/jit/recompile-debug "  old-picmap --> ~a" (if old-picmap (list->set (hash-keys old-picmap)) 'unknown))
  (log-vm/jit/recompile-debug "  pic-infos  --> ~a" (list->set (map car pic-infos)))
  (when stable?
    (log-vm/jit/recompile-info "Compilation of ~a is now stable." (method-name method class)))
  (define inner-code
    `(let ((call-counter 0)
           (cmi #f))
       (case-lambda
         [()
          (when (not cmi)
            (set! cmi
                  (compiled-method-info
                   ,(AbsVal-expr (Ctx-lit c method))
                   ,pic-infos-exp
                   ,stable?)))
          cmi]
         [(,top-k ,@(map AbsVal-expr (vector->list (Ctx-arguments c))))
          ;; (log-vm/jit-debug "Entering ~a with ~a"
          ;;                   ,(method-name method class)
          ;;                   (list ,@(map AbsVal-expr (vector->list (Ctx-arguments c)))))
          (set! call-counter (+ call-counter 1))
          ;; TODO: aging of call-counter by right-shifting at most once every few seconds, or so
          (when (= call-counter 1000)
            (log-vm/jit/recompile-debug "Method ~a is hot" ,(method-name method class))
            (recompile-something vm (,top-k))
            ;; (set! call-counter 0)
            )
          ,(gen-fresh-temps c (gen-label-definitions c body-code))])))
  (finish-compilation c compile-time-vm inner-code))

(define (finish-compilation c vm inner-code)
  (define litmap-list (hash->list (Ctx-litmap c)))
  (define code `(lambda (vm ,@(map cdr litmap-list)) ,inner-code))
  (log-vm/jit/code-debug "Resulting code for ~a:\n~a" (Ctx-name c) (pretty-format code))
  (apply (eval code ns) vm (map car litmap-list)))

;; (define (compile-block-proc compile-time-vm
;;                             method
;;                             outer-args
;;                             actual-temporaries
;;                             argument-location
;;                             initial-ip)
;;   (define class (obj-class* compile-time-vm (car outer-args)))
;;   (define c (top-compilation compile-time-vm class method #f))
;;   (define body-code (gen-block c argument-location initial-ip)) ;; imperative!
;;   (define inner-code
;;     `(lambda (temporaries ,@(map AbsVal-expr (vector->list (compilation-argabsvals c))))
;;        (let ((outer-k (outermost-k vm)))
;;          ,(gen-label-definitions c body-code))))
;;   (apply (finish-compilation c compile-time-vm inner-code)
;;          actual-temporaries
;;          outer-args))

(define (block->thunk vm block args) ;; Expects a real bytecode block, not an ffiv one
  (lambda ()
    (define method (slotAt block 0))
    (define outer-args (vector->list (obj-slots (slotAt block 1))))
    (define temporaries (obj-slots (slotAt block 2)))
    (define argument-location (slotAt block 7))
    (define block-ip (slotAt block 9))
    (error 'block->thunk "Unimplemented")
    ;; (define f (compile-block-proc vm method outer-args temporaries argument-location block-ip))
    ;; (apply f vm (outermost-k vm) args)
    ))

(define (gen-build-jit-context c)
  (if (DynamicCtx? c)
      `(,(DynamicCtx-var c))
      `(build-jit-context vm
                          ,(gen-build-jit-context (Ctx-previous c))
                          (vector ,@(map AbsVal-expr (vector->list (Ctx-arguments c))))
                          ,(AbsVal-expr (Ctx-lit c (Ctx-method c)))
                          ,(Ctx-ip c)
                          ,(Ctx-temporaries c)
                          (vector ,@(map AbsVal-expr (reverse (Ctx-stack c)))))))

(define (gen-fresh-temps c body-code)
  `(let ((,(Ctx-temporaries c)
          ,(match (slotAt (Ctx-method c) 4)
             [0 `'#()]
             [temp-count
              `(make-vector ,temp-count ,(AbsVal-expr (Ctx-lit c (VM-nil (Ctx-vm c)))))])))
     ,body-code))

(define (bytecode-exceeding? method limit)
  (define bytecode (bv-bytes (slotAt method 1)))
  (log-vm/jit/code-debug "Method ~v bytecode length ~a compared against limit ~a"
                         (method-name method)
                         (bytes-length bytecode)
                         limit)
  (> (bytes-length bytecode) limit))

(define (gen-pic c name-bytes extension)
  (define p (if (null? extension)
                (pic)
                (apply extended-pic
                       (flatten (take (append extension empty-pic-extension) pic-entry-count)))))
  (set-State-pic-list-rev! (Ctx-state c) (cons (list c name-bytes p) (State-pic-list-rev (Ctx-state c))))
  (gen-lit* (Ctx-litmap c) p))

;; TODO: record dependency links properly, so that if a method is
;; changed, inlined copies of the old version of the method are
;; discarded.

(define (gen-inline-send kc method arg-avs)
  (define ic
    (inline-compilation (Ctx-vm kc) method (list->vector arg-avs) #f 0 kc #f (Ctx-state kc)))
  (log-vm/jit/code-debug "Inlining send of ~a into method ~a" (Ctx-name ic) (Ctx-name kc))
  (define body-code
    `(begin
       ;; (log-vm/jit/code-debug "Entering inlined send of ~a returning to ~a with ~a"
       ;;                        ,(method-name method)
       ;;                        ,(format-Ctx kc)
       ;;                        (list ,@(map AbsVal-expr arg-avs)))
       ,(gen-fresh-temps ic (gen-label-definitions ic (gen-code ic)))))
  (log-vm/jit/code-debug "INLINED for send of ~a into method ~a:\n~a"
                         (Ctx-name ic)
                         (Ctx-name kc)
                         (pretty-format body-code))
  body-code)

(define (analyse-pic c pic)
  (define unsorted (for/list [(i (in-range (pic-size pic))) #:when (pic@ pic i 0)]
                     (list (pic@ pic i 2) (pic@ pic i 0) (pic@ pic i 1))))
  (define descending-by-call-count (map cdr (sort unsorted > #:key car)))
  (for [(entry descending-by-call-count)]
    (unwrap-cached-method (Ctx-vm c) (cadr entry))) ;; fills cache entry
  descending-by-call-count)

(define (tiny-method? bmethod) (not (bytecode-exceeding? bmethod 32)))
(define (small-method? bmethod) (not (bytecode-exceeding? bmethod 40)))

(define (remaining-basic-block-size-tiny? c)
  (define method (Ctx-method c))
  (define bytecode (bv-bytes (slotAt method 1)))
  (define ip (Ctx-ip c))
  (define remaining-bytes (- (bytes-length bytecode) ip)) ;; TODO: actually figure this out properly
  (log-vm/jit-debug "Evaluating continuation size: ~a bytes left in ~a" remaining-bytes c)
  (not (> remaining-bytes 6)))

(define (Ctx->expr c)
  (if (DynamicCtx? c)
      (DynamicCtx-var c)
      (let ((ans (gensym 'answer)))
        `(case-lambda [() ,(gen-build-jit-context c)]
                      [(,ans)
                       ;; (log-vm/jit-debug "Continuing ~a with ~a" ,(format-Ctx c) ,ans)
                       ,(truncate-histories c (gen-continuation (Ctx-push c (AbsVal ans (Unknown)))))]))))

(define (gen-continuation c)
  (if (remaining-basic-block-size-tiny? c)
      (gen-code c)
      (gen-jump-to-label c)))

(define (gen-send c class-absval name-bytes selector-absval arg-avs kc)
  (log-vm/jit-debug "Send of ~a at ~a returning to ~a" name-bytes c kc)
  (define vm (Ctx-vm c))
  (define class-desc (AbsVal-desc class-absval))
  ;; (log-vm/jit-debug "class-absval is ~a" class-absval)
  (log-vm/jit-debug "arg-avs = ~a" arg-avs)
  (if (Constant? class-desc)
      (let* ((class (Constant-value class-desc))
             (cm (lookup-method/cache vm class name-bytes))
             (bmethod (or (cached-method-bytecode-method cm)
                          (lookup-method vm class name-bytes)
                          (error 'gen-send "DNU at compile time: ~a ~a" class name-bytes))))
        (if (or (already-compiling? c class bmethod)
                (not (tiny-method? bmethod))
                (not (sufficiently-static? c arg-avs)))
            `((unwrap-cached-method vm ,(AbsVal-expr (Ctx-lit c cm)))
              ,(Ctx->expr kc) ,@(map AbsVal-expr arg-avs))
            (gen-inline-send kc bmethod arg-avs)))
      (let ()
        (define old-picmap (State-old-picmap (Ctx-state c)))
        (define old-entry (and old-picmap (hash-ref old-picmap c #f)))
        (define previous-pic-entries (if old-entry (analyse-pic c (cadr old-entry)) '()))
        (define pic-m (gen-pic c name-bytes previous-pic-entries))
        `(let ((k-send ,(Ctx->expr kc)))
           ,(let loop ((predictions previous-pic-entries) (counter pic-entry-count))
              (match predictions
                ['()
                 `((lookup-message/jit vm ,pic-m ,(AbsVal-expr class-absval) ,(AbsVal-expr selector-absval))
                   k-send ,@(map AbsVal-expr arg-avs))]
                [(cons (list predicted-class predicted-cm) more-predictions)
                 (define predicted-bmethod (cached-method-bytecode-method predicted-cm))
                 (define final-arg-avs (augment-receiver-class c arg-avs predicted-class))
                 `(if (eq? ,(AbsVal-expr class-absval) ,(AbsVal-expr (Ctx-lit c predicted-class)))
                      (begin
                        (pic-bump! ,pic-m ,counter)
                        ,(if (or (already-compiling? c predicted-class predicted-bmethod)
                                 (not (small-method? predicted-bmethod))
                                 (not (sufficiently-static? c final-arg-avs)))
                             `((unwrap-cached-method vm ,(AbsVal-expr (Ctx-lit c predicted-cm)))
                               k-send ,@(map AbsVal-expr final-arg-avs))
                             (gen-inline-send kc predicted-bmethod final-arg-avs)))
                      ,(loop more-predictions (+ counter 1)))]))))))

(define (sufficiently-static? c avs)
  (or (andmap (lambda (av) (not (Unknown? (AbsVal-desc av)))) avs)
      (< (Ctx-depth c) 3)))

(define (augment-receiver-class c arg-avs class)
  (match-define (cons (AbsVal expr _desc) rest) arg-avs)
  (cons (AbsVal expr (Obj (Ctx-lit c class) #f)) rest))

(define (gen-block c argument-location)
  (define temp-count (slotAt (Ctx-method c) 4))
  (define block-k (gensym 'block-k))
  (define bc (inline-compilation (Ctx-vm c)
                                 (Ctx-method c)
                                 (Ctx-arguments c)
                                 (Ctx-temporaries c)
                                 (Ctx-ip c)
                                 (DynamicCtx block-k)
                                 (or (Ctx-home c) (Ctx-previous c)) ;; ??
                                 (Ctx-state c)))
  `(lambda (,block-k . block-arguments)
     ;; (log-vm/jit-debug "Entering block at ~a with ~a" ,(format-Ctx bc) block-arguments)
     ,(let loop ((i argument-location))
        (if (>= i temp-count)
            `(void)
            `(when (pair? block-arguments)
               (vector-set! ,(Ctx-temporaries c) ,i (car block-arguments))
               (let ((block-arguments (cdr block-arguments)))
                 ,(loop (+ i 1))))))
     ,(truncate-histories bc (gen-label-definitions bc (gen-code bc)))))

(define (emit* c var purity absval)
  (define param (State-histories (Ctx-state c)))
  (match-define (cons era hs) (param))
  (param (cons (cons (definition var purity absval) era) hs))
  (AbsVal var (AbsVal-desc absval)))

(define (historical-match c purity expr)
  (define param (State-histories (Ctx-state c)))
  (and (eq? purity 'pure)
       (let search-hs ((hs (param)))
         (match hs
           ['() #f]
           [(cons era hs)
            (let search-era ((era era))
              (match era
                ['() (search-hs hs)]
                [(cons (definition var 'pure (AbsVal (== expr) desc)) _) (AbsVal var desc)]
                [(cons _ era) (search-era era)]))]))))

(define-syntax emit
  (syntax-rules ()
    [(_ c-expr [(var vargen) purity absval-expr] body-expr)
     (let* ((c c-expr)
            (absval absval-expr))
       (let ((var (or (historical-match c 'purity (AbsVal-expr absval))
                      (emit* c vargen 'purity absval))))
         body-expr))]
    [(_ c-expr [var purity absval-expr] body-expr)
     (emit c-expr [(var (gensym 'var)) purity absval-expr] body-expr)]))

(define-syntax-rule (residualize c code-expr)
  (let ((param (State-histories (Ctx-state c))))
    (parameterize ((param (cons '() (param))))
      (define code code-expr)
      (wrap-era (car (param)) code (free-names code)))))

(define-syntax-rule (truncate-histories c-expr code-expr)
  (let* ((c c-expr)
         (param (State-histories (Ctx-state c))))
    (parameterize ((param '()))
      (residualize c code-expr))))

(define (wrap-era era body outstanding)
  (match era
    ['() body]
    [(cons (definition var purity (AbsVal expr _desc)) era)
     (if (or (eq? purity 'effect) (set-member? outstanding var))
         (wrap-era era
                   (if (equal? body var) expr `(let ((,var ,expr)) ,body))
                   (set-remove (set-union (free-names expr) outstanding) var))
         (wrap-era era body (set-remove outstanding var)))]))

(define (free-names expr)
  (log-vm-debug "free-names is a hideous overapproximation")
  (match expr
    [(? symbol? n) (seteq n)]
    [`(,exprs ...) (apply set-union (seteq) (map free-names exprs))]
    [_ (seteq)]))

(define (SlotAt c absval index)
  (match (AbsVal-desc absval)
    [(Obj _ (? vector? slot-absvals)) (vector-ref slot-absvals index)]
    [_ (AbsVal `(slotAt ,(AbsVal-expr absval) ,index) (Unknown))]))

(define (ObjClass vm absval)
  (match (AbsVal-desc absval)
    [(Obj cls _) cls]
    [(Constant v) (AbsVal `(obj-class* vm ,(AbsVal-expr absval)) (Constant (obj-class* vm v)))]
    [_ (AbsVal `(obj-class* vm ,(AbsVal-expr absval)) (Unknown))]))

(define (read-opcode read-byte)
  (define byte (read-byte))
  (define low (bitwise-and byte #x0f))
  (define high (bitwise-and (arithmetic-shift byte -4) #x0f))
  (if (zero? high)
      (values low (read-byte))
      (values high low)))

(define (gen-code c)
  (log-vm/jit-debug "gen-code for ~a, stack ~a" c (Ctx-stack c))
  (residualize
   c
   (let translate ((c c))
     (define vm (Ctx-vm c))
     (define method (Ctx-method c))
     (define bytecode (bv-bytes (slotAt method 1)))
     (define literals (slotAt method 2))
     (define c0 c)
     (define stack (Ctx-stack c))
     (define (next-byte!)
       (let ((ip (Ctx-ip c)))
         (begin0 (bytes-ref bytecode ip)
           (set! c (Ctx-goto c (+ ip 1))))))
     (define (decode!) (read-opcode next-byte!))
     (define-values (opcode arg) (decode!))
     (log-vm/jit-debug " ~a: ~a ~a" c0 opcode arg)
     (match opcode
       [1 (emit c [slotval pure (SlotAt c (Ctx-receiver c) arg)] (translate (Ctx-push c slotval)))]
       [2 (translate (Ctx-push c (Ctx-arg c arg)))]
       [3 (emit c [(n (gensym (format "temp~a-" arg)))
                   pure
                   (AbsVal `(vector-ref ,(Ctx-temporaries c) ,arg) (Unknown))]
                (translate (Ctx-push c n)))]
       [4 (translate (Ctx-push c (Ctx-lit c (slotAt literals arg))))]
       [5 (translate (Ctx-push c (Ctx-lit c (match arg
                                              [(or 0 1 2 3 4 5 6 7 8 9) arg]
                                              [10 (VM-nil vm)]
                                              [11 (VM-true vm)]
                                              [12 (VM-false vm)]))))]
       [6 (let ((self-expr (AbsVal-expr (Ctx-receiver c)))
                (val-expr (AbsVal-expr (car stack))))
            (emit c [ignored effect (AbsVal `(slotAtPut ,self-expr ,arg ,val-expr) (Unknown))]
                  (truncate-histories c (translate c))))]
       [7 (let ((val-expr (AbsVal-expr (car stack))))
            (emit c [ignored effect (AbsVal `(vector-set! ,(Ctx-temporaries c) ,arg ,val-expr)
                                            (Unknown))]
                  (truncate-histories c (translate c))))]
       [8 (let* ((arg-count arg)
                 (args (reverse (take stack arg-count))))
            (set! c (Ctx-drop c arg-count))
            (define-values (selector-literal-index class-absval)
              (match/values (decode!)
                [(9 selector-literal-index)
                 (emit c [cls pure (ObjClass vm (car args))]
                       (values selector-literal-index cls))]
                [(15 11)
                 (define super (slotAt (slotAt method 5) 1))
                 (values (next-byte!) (Ctx-lit c super))]))
            (define selector (slotAt literals selector-literal-index))
            (gen-send c0 class-absval (bv-bytes selector) (Ctx-lit c selector) args c))]
       ;; 9 inlined in the processing of bytecode 8
       [10 (match arg
             [0 (emit c [isNil pure
                               (if (equal? (Constant (VM-nil vm)) (AbsVal-desc (car stack)))
                                   (Ctx-lit c (VM-true vm))
                                   (AbsVal `(boolean->obj vm (eq? ,(AbsVal-expr (Ctx-lit c (VM-nil vm)))
                                                                  ,(AbsVal-expr (car stack))))
                                           (Unknown)))]
                      (translate (Ctx-push (Ctx-drop c 1) isNil)))]
             [1 (emit c [notNil pure
                                (if (equal? (Constant (VM-nil vm)) (AbsVal-desc (car stack)))
                                    (Ctx-lit c (VM-false vm))
                                    (AbsVal `(boolean->obj vm (not (eq? ,(AbsVal-expr (Ctx-lit c (VM-nil vm)))
                                                                        ,(AbsVal-expr (car stack)))))
                                            (Unknown)))]
                      (translate (Ctx-push (Ctx-drop c 1) notNil)))])]
       [11 (match stack
             [(list* jv iv _stack)
              ;; TODO: Fix now-unwanted special-casing of these sends. REQUIRES
              ;; IMAGE CHANGES, particularly in `addToSmallInt:`.
              (set! c (Ctx-drop c 2))
              (define i (AbsVal-expr iv))
              (define j (AbsVal-expr jv))
              `(if (and (number? ,i) (number? ,j))
                   ,(residualize c
                                 (emit c [opresult pure (AbsVal (match arg
                                                                  [0 `(boolean->obj vm (< ,i ,j))]
                                                                  [1 `(boolean->obj vm (<= ,i ,j))]
                                                                  [2 `(+ ,i ,j)])
                                                                (Unknown))]
                                       (translate (Ctx-push c opresult))))
                   ,(residualize c
                                 (let ((name-bytes (match arg [0 #"<"] [1 #"<="] [2 #"+"])))
                                   (gen-send c0
                                             (ObjClass vm iv)
                                             name-bytes
                                             (AbsVal `(mkbv ,(AbsVal-expr (Ctx-lit c (VM-nil vm)))
                                                            ,name-bytes)
                                                     (Bv (Ctx-lit c (VM-nil vm))
                                                         (vector)
                                                         name-bytes))
                                             (list iv jv)
                                             c))))])]
       [12 (let ((target (next-byte!))
                 (argument-location arg))
             (emit c [block pure
                            (AbsVal
                             `(mkffiv ,(AbsVal-expr (Ctx-lit c (VM-Block vm)))
                                      ,(gen-block c argument-location))
                             (Ffiv (Ctx-lit c (VM-Block vm))
                                   #f
                                   (let ((c c))
                                     (lambda (kc arg-avs)
                                       (log-vm/jit-debug "Inlining block ~a returning to ~a" c kc)
                                       (define bc
                                         (inline-compilation vm
                                                             method
                                                             (Ctx-arguments c)
                                                             (Ctx-temporaries c)
                                                             (Ctx-ip c)
                                                             kc
                                                             (or (Ctx-home c) (Ctx-previous c)) ;; ??
                                                             (Ctx-state c)))
                                       (for [(i (in-naturals argument-location)) (arg arg-avs)]
                                         (define av
                                           (AbsVal `(vector-set! ,(Ctx-temporaries c)
                                                                 ,i
                                                                 ,(AbsVal-expr arg))
                                                   (Unknown)))
                                         (emit bc [blkarg effect av] (void)))
                                       (truncate-histories
                                        bc
                                        (gen-label-definitions bc (gen-code bc)))))))]
                   (translate (Ctx-push-and-goto c target block))))]
       [13 (define primitive-number (next-byte!))
           (define primitive-arg-count arg)
           (define primitive-args (reverse (take stack primitive-arg-count)))
           (set! c (Ctx-drop c arg))
           (match primitive-number
             [2 (emit c [primcls pure (ObjClass vm (car primitive-args))]
                      (translate (Ctx-push c primcls)))]
             [7 (match-define (list class count) primitive-args)
                (emit c [(obj (gensym (class-temp-name class)))
                         effect
                         (AbsVal `(obj ,(AbsVal-expr class)
                                       (make-vector ,(AbsVal-expr count)
                                                    ,(AbsVal-expr (Ctx-lit c (VM-nil vm)))))
                                 (Obj class #f))]
                      (translate (Ctx-push c obj)))]
             [8 (let ((v (gensym 'blockresult))
                      (block (last primitive-args))
                      (argc (- arg 1))
                      (primitive-args (reverse (cdr (reverse primitive-args)))))
                  (log-vm/jit-debug "Attempt to invoke block ~a" block)
                  (if (and (Ffiv? (AbsVal-desc block))
                           (equal? (Constant (VM-Block vm)) (AbsVal-desc (ObjClass vm block))))
                      ;; NB relies on tail call effect of primitive 8 (!)
                      ((Ffiv-value (AbsVal-desc block)) (Ctx-previous c) primitive-args)
                      `(match ,(AbsVal-expr block)
                         [(unffiv block-proc)
                          (block-proc
                           ;; TODO vvv : use case-lambda to translate the context chain
                           ,(Ctx->expr (Ctx-previous c)) ;; not ,(Ctx->expr c)
                           ;; ^ reason being the image BUGGILY (?!?) relies on primitive 8
                           ;; immediately returning to the surrounding context!!
                           ,@(map AbsVal-expr primitive-args))]
                         [(obj (== ,(AbsVal-expr (Ctx-lit c (VM-Block vm)))) _)
                          (log-vm/jit-warning "Unoptimized block!")
                          ,(let ((expr `((block->thunk vm
                                                       ,(AbsVal-expr block)
                                                       (list ,@(map AbsVal-expr primitive-args))))))
                             (match (Ctx-previous c)
                               [(DynamicCtx dk)
                                `(,dk ,expr)]
                               [caller
                                (gen-code (Ctx-push caller (AbsVal expr (Unknown))))]))])))]
             [34 (Ctx-lit c (VM-nil vm))]
             [35 (emit c [ctxref pure (AbsVal (gen-build-jit-context c) (Unknown))]
                       (translate (Ctx-push c ctxref)))]
             [36 (emit c [arr effect (AbsVal `(mkobj ,(AbsVal-expr (Ctx-lit c (VM-Array vm)))
                                                     ,@(map AbsVal-expr primitive-args))
                                             (Obj (Ctx-lit c (VM-Array vm))
                                                  (list->vector primitive-args)))]
                       (translate (Ctx-push c arr)))]
             [_ (let ((generator (hash-ref *primitive-code-snippets*
                                           primitive-number
                                           (lambda () (error 'gen-code
                                                             "Unknown primitive: ~a"
                                                             primitive-number)))))
                  (emit c [primresult effect
                                      (AbsVal (generator 'vm (map AbsVal-expr primitive-args))
                                              (Unknown))]
                        (translate (Ctx-push c primresult))))])]
       [14 (emit c [clsvar pure (SlotAt c (ObjClass vm (Ctx-receiver c)) (+ arg 5))]
                 (translate (Ctx-push c clsvar)))]
       [15 (define (continue c av)
             (match c
               [(DynamicCtx dk) `(,dk ,(AbsVal-expr av))]
               [_ (translate (Ctx-push c av))]))
           (match arg
             [1 (continue (Ctx-previous c) (Ctx-receiver c))]
             [2 (continue (Ctx-previous c) (car stack))]
             [3 (continue (Ctx-home c) (car stack))]
             [5 (translate (Ctx-drop c 1))]
             [6 (gen-jump-to-label (Ctx-goto c (next-byte!)))]
             [7 (let ((target (next-byte!))
                      (disc (car stack)))
                  (set! c (Ctx-drop c 1))
                  (log-vm/jit-debug "if ~a true jump to ~a, else continue at ~a" disc target (Ctx-ip c))
                  (if (equal? (Constant (VM-true vm)) (AbsVal-desc disc))
                      (gen-code (Ctx-goto c target))
                      `(if (eq? ,(AbsVal-expr disc) ,(AbsVal-expr (Ctx-lit c (VM-true vm))))
                           ,(gen-continuation (Ctx-goto c target))
                           ,(gen-continuation c))))]
             [8 (let ((target (next-byte!))
                      (disc (car stack)))
                  (set! c (Ctx-drop c 1))
                  (log-vm/jit-debug "if ~a false jump to ~a, else continue at ~a" disc target (Ctx-ip c))
                  (if (equal? (Constant (VM-false vm)) (AbsVal-desc disc))
                      (gen-code (Ctx-goto c target))
                      `(if (eq? ,(AbsVal-expr disc) ,(AbsVal-expr (Ctx-lit c (VM-false vm))))
                           ,(gen-continuation (Ctx-goto c target))
                           ,(gen-continuation c))))]
             ;; 11 inlined in the processing of bytecode 8
             [_ (error 'gen-code "Unhandled do-special case ~v" arg)])]
       [_ (error 'gen-code "~a - unhandled opcode ~v, arg ~v" (Ctx-name c) opcode arg)]))))

(define (class-temp-name av)
  (match (AbsVal-desc av)
    [(Constant (obj _ (vector (? bv? name) _ ...)))
     (string-append "new" (bv->string name))]
    [_
     "newobj"]))

;; (define (dump-full-context c)
;;   (log-vm/jit-debug "FULL CONTEXT:")
;;   (let loop ((c c))
;;     (log-vm/jit-debug "  ~a: stack ~a" c (Ctx-stack c))
;;     (cond [(Ctx-previous c) => loop]
;;           [else (void)]))
;;   (log-vm/jit-debug "HISTORIES: ~a" ((State-histories (Ctx-state c)))))

(define (gen-jump-to-label c)
  (define labels (Ctx-labels c))
  (define key (Ctx-ip c))
  (when (not (hash-has-key? labels key))
    (define var
      (gensym (mksym "label-~a-~a-"
                     (bv->string (slotAt (Ctx-method c) 0))
                     (Ctx-ip c))))
    (hash-set! labels key (cons 'placeholder var))
    (define newstack (for/list [(i (length (Ctx-stack c)))] (AbsVal (mksym "stack~a" i) (Unknown))))
    (log-vm/jit-debug "Producing label ~a" var)
    (define bb-k (gensym 'bb-k))
    (define expr (truncate-histories
                  c
                  (let* ((c (Ctx-update c (Ctx-ip c) (lambda (_s) newstack)))
                         (c (struct-copy Ctx c [previous (DynamicCtx bb-k)])))
                    ;; (dump-full-context c)
                    (gen-code c))))
    (log-vm/jit-debug "Produced label ~a" var)
    (hash-set! labels key (cons `(lambda (,bb-k ,@(map AbsVal-expr newstack)) ,expr) var)))
  `(,(cdr (hash-ref labels key))
    ,(Ctx->expr (Ctx-previous c))
    ,@(map AbsVal-expr (Ctx-stack c))))

(define (gen-label-definitions c body-exp)
  `(letrec (,@(for/list [(label-info (in-hash-values (Ctx-labels c)))]
                (match-define (cons label-exp var) label-info)
                (log-vm/jit-debug "Emitting label ~a" var)
                `(,var ,label-exp)))
     ,body-exp))

(define (outermost-k vm)
  (case-lambda [() (VM-nil vm)]
               [(result) result]))

;;===========================================================================
;; Recompilation

(define (recompilation-candidate vm ctx)
  (let search ((ctx ctx) (candidate #f) (candidate-class #f) (candidate-hotness 0))
    (cond
      [(eq? (VM-nil vm) ctx) (values candidate candidate-class)]
      [else (define method (slotAt ctx 0))
            (define receiver (slotAt (slotAt ctx 1) 0))
            (define receiver-class (obj-class* vm receiver))
            (define next-ctx (slotAt ctx 6))
            (log-vm/jit/recompile/candidates-debug "  ~a" (method-name method receiver-class))
            (define cached-method (bytecode->cached-compiled vm receiver-class method))
            (define compiled-method (unwrap-cached-method vm cached-method))
            (cond
              [(not compiled-method) (search next-ctx candidate candidate-class candidate-hotness)]
              [else
               (match-define (compiled-method-info (== method eq?) pics stable?) (compiled-method))
               (log-vm/jit/recompile/candidates-debug "    has ~a bytes of bytecode; ~a"
                                                      (bytes-length (bv-bytes (slotAt method 1)))
                                                      (if stable? "stable" "not yet stable"))
               (define hotness
                 (for/sum [(entry pics)]
                   (match-define (list c _name-bytes pic) entry)
                   (for/sum [(i (in-range (pic-size pic)))]
                     (match (pic@ pic i 0)
                       [#f 0]
                       [slot-class
                        (define slot-cm (pic@ pic i 1))
                        (unwrap-cached-method vm slot-cm) ;; fills cache entry
                        (define slot-bmethod (cached-method-bytecode-method slot-cm))
                        (define slot-count (pic@ pic i 2))
                        (define bytecode-count (bytes-length (bv-bytes (slotAt slot-bmethod 1))))
                        (define weight (/ 40.0 bytecode-count))
                        (log-vm/jit/recompile/candidates-debug
                         "      context ~a class ~a count ~a length ~a weight ~a"
                         c
                         (bv->string (slotAt slot-class 0))
                         slot-count
                         bytecode-count
                         weight)
                        (if (< weight 1)
                            0
                            (* slot-count weight))]))))
               (log-vm/jit/recompile/candidates-debug "    hotness: ~a" hotness)
               (if (and (> hotness candidate-hotness) (not stable?))
                   (search next-ctx method receiver-class hotness)
                   (search next-ctx candidate candidate-class candidate-hotness))])])))

(define (recompile-method! vm class method)
  (log-vm/jit/recompile-info "Recompiling ~a" (method-name method class))
  (define cached-method (bytecode->cached-compiled vm class method))
  (define old-proc (cached-method-proc cached-method))
  (define old-picmap
    (for/hash [(entry (in-list (if old-proc (compiled-method-info-pics (old-proc)) '())))]
      (values (car entry) (cdr entry))))
  (when (not (hash-empty? old-picmap))
    (log-vm/jit/recompile-info "Retrieved old pics for method ~a" (method-name method class))
    (for [((c p) (in-hash old-picmap))]
      (log-vm/jit/recompile-info "   ~a --> ~v" (format-Ctx c) p)))
  (define recompiled-proc (compile-method-proc vm class method old-picmap))
  (log-vm/jit/recompile-info "Updating cached compiled method for ~a" (method-name method class))
  (set-cached-method-proc! cached-method recompiled-proc))

(define (recompile-something vm ctx)
  (define-values (candidate candidate-class) (recompilation-candidate vm ctx))
  (if candidate
      (recompile-method! vm candidate-class candidate)
      (log-vm/jit/recompile-debug "No recompilation candidate available?")))

;;===========================================================================
;; VM-specific primitives (aside from the core primitives found in `gen-code`)

(define-primitive vm [6 inner-ctx] ;; "new context execute"
  (when (not (zero? (slotAt inner-ctx 5))) (error 'execute "Cannot execute from nonempty stack"))
  (when (not (zero? (slotAt inner-ctx 4))) (error 'execute "Cannot execute from nonzero IP"))
  (define args (slotAt inner-ctx 1))
  (define f (compile-method-proc vm (obj-class* vm (slotAt args 0)) (slotAt inner-ctx 0) #f))
  (apply f (outermost-k vm) (vector->list (obj-slots args))))

(define-primitive vm [116] (save-image-to-file vm (pe-VM-image-filename vm)))

;;===========================================================================
;; Entry point

(pretty-print-columns 230)
(let* ((image-filename "SmallWorld/src/image")
       (vm (call-with-input-file image-filename
             (lambda (fh)
               (read-image fh pe-VM (list (make-weak-hasheq) image-filename))))))
  (boot-image vm
              (lambda (vm source)
                (define compiled-method
                  (unwrap-cached-method vm (lookup-method/cache vm (obj-class source) #"doIt")))
                (compiled-method (outermost-k vm) source))
              (current-command-line-arguments)))
