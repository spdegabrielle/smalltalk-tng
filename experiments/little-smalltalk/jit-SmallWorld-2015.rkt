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

(define pic-reserved 0)
(define pic-entry-count 3)
(define (pic)
  ;; pic-entry-count times three - one each for class, method, and count.
  (vector #f #f 0
          #f #f 0
          #f #f 0))
(define (extended-pic c0 m0 c1 m1 c2 m2)
  (vector #f #f 0
          #f #f 0
          #f #f 0
          c0 m0 0
          c1 m1 0
          c2 m2 0))
(define (pic-size pic) (quotient (- (vector-length pic) pic-reserved) pic-entry-count))
(define empty-pic-extension (for/list [(i (in-range pic-entry-count))] '(#f #f)))
(define (pic@ pic index offset) (vector-ref pic (+ pic-reserved offset (* index 3))))
(define (pic@! pic index offset v) (vector-set! pic (+ pic-reserved offset (* index 3)) v))
(define (pic-bump! pic index)
  (define o (+ pic-reserved 2 (* index 3)))
  (vector-set! pic o (+ 1 (vector-ref pic o))))

(struct jit-VM VM (cache image-filename)
  #:methods gen:vm-callback
  [(define (vm-block-callback vm action)
     ;; Runs action in a new thread
     (lambda args
       (thread (match action
                 [(unffiv block-proc)
                  (lambda () (apply block-proc vm (outermost-k vm) args))]
                 [_
                  (block->thunk vm action args)]))))])

(struct pic-info (name-bytes variable context extension) #:transparent)
(struct compilation-result (litmap [pic-list-rev #:mutable] old-picmap))
(struct compilation (outer outer-ip vm receiver-class method argnames labels state))

(struct compiled-method-info (bytecode-method pics stable?))

(struct cached-method (class name-bytes [bytecode-method #:mutable] [proc #:mutable]))

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

(define (selector-string-arity str)
  (define colon-count (for/sum [(c str)] (if (eqv? c #\:) 1 0)))
  (cond [(positive? colon-count) (+ colon-count 1)]
        [(char-alphabetic? (string-ref str 0)) 1]
        [else 2])) ;; assume binary operator

(define-namespace-anchor ns-anchor)
(define ns (namespace-anchor->namespace ns-anchor))

(define (mksym fmt . args) (string->symbol (apply format fmt args)))

(define-syntax let@
  (syntax-rules ()
    [(_ [n n-code-exp] body-code-exp)
     (let@ [n 'n n-code-exp] body-code-exp)]
    [(_ [n n-exp n-code-exp] body-code-exp)
     (let ((n (gensym n-exp)))
       `(let ((,n ,n-code-exp))
          ,body-code-exp))]))

(define (method-name method [class #f])
  (if class
      (format "~a >> ~a"
              (bv->string (slotAt class 0))
              (bv->string (slotAt method 0)))
      (bv->string (slotAt method 0))))

(define (compilation-method-name c)
  (method-name (compilation-method c) (compilation-receiver-class c)))

(define (compilation-depth c)
  (define o (compilation-outer c))
  (if o (+ 1 (compilation-depth o)) 0))

(define (compilation* outer outer-ip compile-time-vm receiver-class method state)
  (define arity (selector-string-arity (method-name method)))
  (define literals (slotAt method 2))

  (define litmap (compilation-result-litmap state))
  (for [(lit (obj-slots literals))] (gen-lit* litmap lit))

  (define argnames (for/vector [(i arity)] (if (zero? i) 'self (mksym "arg~a" (- i 1)))))

  (define c (compilation outer
                         outer-ip
                         compile-time-vm
                         receiver-class
                         method
                         argnames
                         (make-hash)
                         state))
  (log-vm/jit/code-info
   "Compiling ~a defined in ~v (depth ~a), arity ~a, literals ~a, bytecode ~a, text:\n----\n~a\n----"
   (method-name method receiver-class)
   (slotAt method 5)
   (compilation-depth c)
   arity
   literals
   (bytes->hex-string (bv-bytes (slotAt method 1)))
   (bv->string (slotAt method 6)))
  c)

(define (top-compilation vm receiver-class method old-picmap)
  (compilation* #f #f vm receiver-class method (compilation-result (make-hasheq) '() old-picmap)))

(define (inline-compilation c c-ip receiver-class method)
  (compilation* c c-ip (compilation-vm c) receiver-class method (compilation-state c)))

(define (gen-lit* litmap lit)
  (hash-ref! litmap lit (lambda ()
                          (define n (hash-count litmap))
                          (if (bv? lit)
                              (mksym "lit~a-~a" n (bv->string lit))
                              (mksym "lit~a" n)))))

(define (gen-jump-to-label c ip stack)
  (define labels (compilation-labels c))
  (when (not (hash-has-key? labels ip))
    (hash-set! labels ip 'placeholder)
    (define actual-label
      (let ((newstack (for/list [(i (length stack))] (mksym "stack~a" i))))
        `(lambda (k ,@newstack) ,(gen-code c ip newstack))))
    (hash-set! labels ip actual-label))
  `(,(mksym "label~a" ip) k ,@stack))

(define (gen-build-jit-context c ip stack)
  `(build-jit-context vm
                      (k)
                      (vector ,@(vector->list (compilation-argnames c)))
                      method
                      ,ip
                      temporaries
                      (vector ,@(reverse stack))))

(define (gen-send-k c ip stack)
  (define result (gensym 'result))
  `(case-lambda [() ,(gen-build-jit-context c ip stack)]
                [(,result) ,(gen-jump-to-label c ip (cons result stack))]))

(define (gen-fresh-temps method)
  (match (slotAt method 4)
    [0 `'#()]
    [temp-count `(make-vector ,temp-count NIL)]))

(define (inlineable-self-send? method)
  (define bytecode (bv-bytes (slotAt method 1)))
  (<= (bytes-length bytecode) 32))

(define (compilation-context c ip)
  (if (not c)
      '()
      (cons (list (compilation-receiver-class c) (compilation-method c) ip)
            (compilation-context (compilation-outer c) (compilation-outer-ip c)))))

(define (gen-pic c name-bytes send-ip extension)
  (define old-pics (compilation-result-pic-list-rev (compilation-state c)))
  (define pic-index (length old-pics))
  (define m (mksym "pic~a" pic-index))
  (define pi (pic-info name-bytes m (compilation-context c send-ip) extension))
  (set-compilation-result-pic-list-rev! (compilation-state c) (cons pi old-pics))
  (log-vm/jit/recompile-debug "Produced pic at ip ~a for send of ~a in method ~a"
                              send-ip
                              name-bytes
                              (compilation-method-name c))
  m)

(define (gen-inline-send c c-ip class method k-exp arg-exps)
  (log-vm/jit/code-info "Inlining send of ~a into method ~a"
                        (method-name method class)
                        (compilation-method-name c))
  (define ic (inline-compilation c c-ip class method))
  (define body-code (gen-jump-to-label ic 0 '()))
  (define defining-class (slotAt method 5))
  (define litmap (compilation-result-litmap (compilation-state ic)))
  (define inner-code
    `(let ((k ,k-exp)
           (method ,(gen-lit* litmap method))
           (super ,(gen-lit* litmap (slotAt defining-class 1))))
       (let ,(for/list [(formal (vector->list (compilation-argnames ic)))
                        (actual (in-list arg-exps))]
               `(,formal ,actual))
         (let ((outer-k k)
               (temporaries ,(gen-fresh-temps method)))
           ,(gen-label-definitions ic body-code)))))
  (log-vm/jit/code-debug "INLINED:\n~a" (pretty-format inner-code))
  inner-code)

(define (analyse-pic c pic)
  (define vm (compilation-vm c))
  (define unsorted (for/list [(i (in-range (pic-size pic))) #:when (pic@ pic i 0)]
                     (list (pic@ pic i 2) (pic@ pic i 0) (pic@ pic i 1))))
  (define descending-by-call-count (map cdr (sort unsorted > #:key car)))
  (for [(entry descending-by-call-count)]
    (unwrap-cached-method vm (cadr entry))) ;; fills cache entry
  descending-by-call-count)

(define (already-compiling? c class method)
  (let check ((c c))
    (cond [(not c) #f]
          [(and (eq? (compilation-receiver-class c) class) (eq? (compilation-method c) method)) #t]
          [else (check (compilation-outer c))])))

(define (gen-send c send-ip class-exp name-bytes selector-exp k-exp arg-exps)
  (define receiver-class (compilation-receiver-class c))
  (define method (lookup-method (compilation-vm c) receiver-class name-bytes))
  (cond
    [(and (equal? class-exp `(obj-class* vm self)) ;; self send
          (< (compilation-depth c) 2)
          method
          (inlineable-self-send? method))
     (gen-inline-send c send-ip receiver-class method k-exp arg-exps)]
    [else
     (define old-picmap (compilation-result-old-picmap (compilation-state c)))
     (define old-entry
       (and old-picmap (hash-ref old-picmap (compilation-context c send-ip) #f)))
     (define previous-pic-entries (if old-entry (analyse-pic c (cdr old-entry)) '()))
     (define litmap (compilation-result-litmap (compilation-state c)))
     (define pic-m (gen-pic c name-bytes send-ip previous-pic-entries))
     `(let ((actual-class ,class-exp)
            (k-send ,k-exp))
        ,(let loop ((predictions previous-pic-entries) (counter pic-entry-count))
           (match predictions
             ['()
              `((lookup-message/jit vm ,pic-m actual-class ,selector-exp) vm k-send ,@arg-exps)]
             [(cons (list predicted-class predicted-cm) more-predictions)
              (define predicted-bmethod (cached-method-bytecode-method predicted-cm))
              `(if (eq? actual-class ,(gen-lit* litmap predicted-class))
                   (begin
                     (pic-bump! ,pic-m ,counter)
                     ,(if (already-compiling? c predicted-class predicted-bmethod)
                          `((unwrap-cached-method vm ,(gen-lit* litmap predicted-cm))
                            vm k-send ,@arg-exps)
                          (gen-inline-send c send-ip predicted-class predicted-bmethod 'k-send arg-exps)))
                   ,(loop more-predictions (+ counter 1)))])))]))

(define (gen-block c argument-location ip)
  (define temp-count (slotAt (compilation-method c) 4))
  `(lambda (vm k . block-arguments)
     ,(let loop ((i argument-location))
        (if (>= i temp-count)
            `(void)
            `(when (pair? block-arguments)
               (vector-set! temporaries ,i (car block-arguments))
               (let ((block-arguments (cdr block-arguments)))
                 ,(loop (+ i 1))))))
     ,(gen-code c ip '())))

(define (compilation-litname c literal)
  (hash-ref (compilation-result-litmap (compilation-state c)) literal))

(define (has-blocks? method)
  (define bytecode (bv-bytes (slotAt method 1)))
  (define max-ip (bytes-length bytecode))
  (define ip 0)
  (define (next-byte!)
    (begin0 (bytes-ref bytecode ip)
      (set! ip (+ ip 1))))
  (define (decode!)
    (define byte (next-byte!))
    (define low (bitwise-and byte #x0f))
    (define high (bitwise-and (arithmetic-shift byte -4) #x0f))
    (if (zero? high)
        (values low (next-byte!))
        (values high low)))
  (let search ()
    (if (>= ip max-ip)
        #f
        (let-values (((opcode arg) (decode!)))
          (match opcode
            [12 #t]
            [13 (next-byte!) (search)]
            [15 (match arg
                  [6 (next-byte!) (search)]
                  [7 (next-byte!) (search)]
                  [8 (next-byte!) (search)]
                  [11 (next-byte!) (search)]
                  [_ (search)])]
            [_ (search)])))))

(define (gen-code c ip stack)
  (define method (compilation-method c))
  (define bytecode (bv-bytes (slotAt method 1)))
  (define literals (slotAt method 2))
  (let translate ((ip ip) (stack stack))
    (define (next-byte!)
      (begin0 (bytes-ref bytecode ip)
        (set! ip (+ ip 1))))
    (define (decode!)
      (define byte (next-byte!))
      (define low (bitwise-and byte #x0f))
      (define high (bitwise-and (arithmetic-shift byte -4) #x0f))
      (if (zero? high)
          (values low (next-byte!))
          (values high low)))
    (define ip0 ip)
    (define-values (opcode arg) (decode!))
    (log-vm/jit-debug " ~a: ~a ~a" ip0 opcode arg)
    (match opcode
      [1 (let@ [n (mksym "slot~a_" arg) `(slotAt self ,arg)]
               (translate ip (cons n stack)))]
      [2 (translate ip (cons (vector-ref (compilation-argnames c) arg) stack))]
      [3 (let@ [n (mksym "tmp~a_" arg) `(vector-ref temporaries ,arg)]
               (translate ip (cons n stack)))]
      [4 (let ((name (compilation-litname c (slotAt literals arg))))
           (translate ip (cons name stack)))]
      [5 (match arg
           [(or 0 1 2 3 4 5 6 7 8 9) (translate ip (cons arg stack))]
           [10 (translate ip (cons `NIL stack))]
           [11 (translate ip (cons `TRUE stack))]
           [12 (translate ip (cons `FALSE stack))])]
      [6 `(begin (slotAtPut self ,arg ,(car stack)) ,(translate ip stack))]
      [7 `(begin (vector-set! temporaries ,arg ,(car stack)) ,(translate ip stack))]
      [8 (let* ((arg-count arg)
                (args (reverse (take stack arg-count)))
                (stack (drop stack arg-count)))
           (define-values (selector-literal-index class-exp)
             (match/values (decode!)
               [(9 selector-literal-index)
                (values selector-literal-index `(obj-class* vm ,(car args)))]
               [(15 11)
                (values (next-byte!) `super)]))
           (define k (gen-send-k c ip stack))
           (define selector (slotAt literals selector-literal-index))
           (define selector-exp (compilation-litname c selector))
           (gen-send c ip0 class-exp (bv-bytes selector) selector-exp k args))]
      ;; 9 inlined in the processing of bytecode 8
      [10 (match arg
            [0 (let@ [isNil `(boolean->obj vm (eq? NIL ,(car stack)))]
                     (translate ip (cons isNil (cdr stack))))]
            [1 (let@ [notNil `(boolean->obj vm (not (eq? NIL ,(car stack))))]
                     (translate ip (cons notNil (cdr stack))))])]
      [11 (match stack
            [(list* j i stack)
             (let@ [binop-k (gen-send-k c ip stack)]
                   `(if (and (number? ,i) (number? ,j))
                        ,(match arg
                           [0 `(,binop-k (boolean->obj vm (< ,i ,j)))]
                           [1 `(,binop-k (boolean->obj vm (<= ,i ,j)))]
                           [2 `(,binop-k (+ ,i ,j))])
                        ,(let ((name-bytes (match arg [0 #"<"] [1 #"<="] [2 #"+"])))
                           (gen-send c
                                     ip0
                                     `(obj-class* vm ,i)
                                     name-bytes
                                     `(mkbv NIL ,name-bytes)
                                     binop-k
                                     (list i j)))))])]
      [12 (let ((target (next-byte!)))
            (let@ [block `(mkffiv BLOCK ,(gen-block c arg ip))]
                  (translate target (cons block stack))))]
      [13 (define primitive-number (next-byte!))
          (match primitive-number
            [8 (let ((v (gensym 'blockresult))
                     (block (car stack))
                     (argc (- arg 1))
                     (stack (cdr stack)))
                 `(match ,block
                    [(unffiv block-proc)
                     (block-proc vm
                                 ;; TODO vvv : use case-lambda to translate the context chain
                                 k ;; not (lambda (,v) ,(translate ip (cons v (drop stack argc))))
                                 ;; ^ reason being the image BUGGILY (?!?) relies on primitive 8
                                 ;; immediately returning to the surrounding context!!
                                 ,@(reverse (take stack argc)))]
                    [(obj (== BLOCK) _)
                     (k ((block->thunk vm ,block (list ,@(reverse (take stack argc))))))]))]
            [34 'NIL]
            [35 (let@ [ctxref (gen-build-jit-context c ip stack)]
                      (translate ip (cons ctxref stack)))]
            [36 (let@ [arr `(mkobj ARRAY ,@(reverse (take stack arg)))]
                      (translate ip (cons arr (drop stack arg))))]
            [_ (let ((generator (hash-ref *primitive-code-snippets*
                                          primitive-number
                                          (lambda () (error 'gen-code
                                                            "Unknown primitive: ~a"
                                                            primitive-number)))))
                 (let@ [primresult (generator 'vm (reverse (take stack arg)))]
                       (translate ip (cons primresult (drop stack arg)))))])]
      [14 (let@ [clsvar `(slotAt (obj-class* vm self) ,(+ arg 5))]
                (translate ip (cons clsvar stack)))]
      [15 (match arg
            [1 `(k self)]
            [2 `(k ,(car stack))]
            [3 `(outer-k ,(car stack))]
            [5 (translate ip (cdr stack))]
            [6 (gen-jump-to-label c (next-byte!) stack)]
            [7 (let ((target (next-byte!)))
                 (log-vm/jit-debug "if ~a true jump to ~a, else continue at ~a" (car stack) target ip)
                 `(if (eq? ,(car stack) TRUE)
                      ,(gen-jump-to-label c target (cdr stack))
                      ,(gen-jump-to-label c ip (cdr stack))))]
            [8 (let ((target (next-byte!)))
                 (log-vm/jit-debug "if ~a false jump to ~a, else continue at ~a" (car stack) target ip)
                 `(if (eq? ,(car stack) FALSE)
                      ,(gen-jump-to-label c target (cdr stack))
                      ,(gen-jump-to-label c ip (cdr stack))))]
            ;; 11 inlined in the processing of bytecode 8
            [_ (error 'gen-code "Unhandled do-special case ~v" arg)])]
      [_ (error 'gen-code "Method ~v - unhandled opcode ~v, arg ~v"
                (slotAt (compilation-method c) 0) ;; selector
                opcode
                arg)])))

(define (gen-label-definitions c body-exp)
  `(letrec (,@(for/list [((ip label) (in-hash (compilation-labels c)))]
                `(,(mksym "label~a" ip) ,label)))
     ,body-exp))

(define (finish-compilation c compile-time-vm inner-code)
  (define litmap (compilation-result-litmap (compilation-state c)))
  (define pic-definitions
    (for/list [(pi (reverse (compilation-result-pic-list-rev (compilation-state c))))]
      (define extension (pic-info-extension pi))
      `(define ,(pic-info-variable pi)
         ,(if (null? extension)
              `(pic)
              `(extended-pic
                ,@(append-map (lambda (entry)
                                (list (and (car entry) (gen-lit* litmap (car entry)))
                                      (and (cadr entry) (gen-lit* litmap (cadr entry)))))
                              (take (append extension empty-pic-extension) pic-entry-count)))))))
  (define litmap-list (hash->list litmap))
  (define code
    `(lambda (method super NIL TRUE FALSE ARRAY BLOCK ,@(map cdr litmap-list))
       ,@pic-definitions
       ,inner-code))

  (log-vm/jit/code-debug "Resulting code for ~a:\n~a"
                         (compilation-method-name c)
                         (pretty-format code))
  (define literals (slotAt (compilation-method c) 2))
  (define defining-class (slotAt (compilation-method c) 5))
  (apply (eval code ns)
         (compilation-method c)
         (slotAt defining-class 1) ;; defining class's superclass
         (VM-nil compile-time-vm) ;; assuming this VM is the one that will be used at call time!
         (VM-true compile-time-vm)
         (VM-false compile-time-vm)
         (VM-Array compile-time-vm)
         (VM-Block compile-time-vm)
         (map car litmap-list)))

(define (compile-block-proc compile-time-vm
                            method
                            outer-args
                            actual-temporaries
                            argument-location
                            initial-ip)
  (define class (obj-class* compile-time-vm (car outer-args)))
  (define c (top-compilation compile-time-vm class method #f))
  (define body-code (gen-block c argument-location initial-ip)) ;; imperative!
  (define inner-code
    `(lambda (temporaries ,@(vector->list (compilation-argnames c)))
       (let ((outer-k (outermost-k vm)))
         ,(gen-label-definitions c body-code))))
  (apply (finish-compilation c compile-time-vm inner-code)
         actual-temporaries
         outer-args))

(define (bytecode->cached-compiled vm class method)
  (lookup-method/cache vm class (bv-bytes (slotAt method 0))))

(define (compiled->bytecode cmethod)
  (compiled-method-info-bytecode-method (cmethod)))

(define (recompilation-candidate vm ctx)
  (let search ((ctx ctx) (candidate #f) (candidate-class #f) (candidate-hotness 0))
    (cond
      [(eq? (VM-nil vm) ctx) (values candidate candidate-class)]
      [else (define method (slotAt ctx 0))
            (define receiver (slotAt (slotAt ctx 1) 0))
            (define receiver-class (obj-class* vm receiver))
            (define next-ctx (slotAt ctx 6))
            (log-vm/jit/recompile-debug "  ~a" (method-name method receiver-class))
            (define cached-method (bytecode->cached-compiled vm receiver-class method))
            (define compiled-method (unwrap-cached-method vm cached-method))
            (cond
              [(not compiled-method) (search next-ctx candidate candidate-class candidate-hotness)]
              [else
               (match-define (compiled-method-info bytecode-method pics stable?) (compiled-method))
               (log-vm/jit/recompile-debug "    has ~a bytes of bytecode; ~a; ~a"
                                           (bytes-length (bv-bytes (slotAt bytecode-method 1)))
                                           (if (has-blocks? bytecode-method)
                                               "HAS SOME BLOCKS"
                                               "no blocks")
                                           (if stable? "stable" "not yet stable"))
               (define (pic-entry-has-any-calls? entry)
                 (define pic (cdr entry))
                 (for/or [(i (in-range (pic-size pic)))] (positive? (pic@ pic i 2))))
               (define used-pics (filter pic-entry-has-any-calls? pics))
               (define hotness
                 (for/sum [(entry used-pics)]
                   (match-define (cons pi pic) entry)
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
                        (log-vm/jit/recompile-debug
                         "      ~a context ~a class ~a count ~a length ~a weight ~a"
                         (pic-info-name-bytes pi)
                         (pic-info-context pi)
                         (bv->string (slotAt slot-class 0))
                         slot-count
                         bytecode-count
                         weight)
                        (* slot-count weight)]))))
               (log-vm/jit/recompile-debug "    hotness: ~a" hotness)
               (if (and (> hotness candidate-hotness) (not stable?))
                   (search next-ctx method receiver-class hotness)
                   (search next-ctx candidate candidate-class candidate-hotness))])])))

(define (format-compilation-context x)
  (string-join (reverse
                (map (match-lambda [(list c m ip) (format "~a @~a" (method-name m c) ip)]) x))
               ","
               #:before-first "["
               #:after-last "]"))

(define (recompile-method! vm class method)
  (log-vm/jit/recompile-info "Recompiling ~a" (method-name method class))
  (define cached-method (bytecode->cached-compiled vm class method))
  (define old-proc (cached-method-proc cached-method))
  (define old-picmap
    (for/hash [(entry (in-list (if old-proc (compiled-method-info-pics (old-proc)) '())))]
      (define pi (car entry))
      (values (pic-info-context pi) entry)))
  (when (not (hash-empty? old-picmap))
    (log-vm/jit/recompile-info "Retrieved old pics for method ~a" (method-name method class))
    (for [((i p) (in-hash old-picmap))]
      (log-vm/jit/recompile-info "   ~a --> ~v" (format-compilation-context i) p)))
  (define recompiled-proc (compile-method-proc vm class method old-picmap))
  (log-vm/jit/recompile-info "Updating cached compiled method for ~a" (method-name method class))
  (set-cached-method-proc! cached-method recompiled-proc))

(define (recompile-something vm ctx)
  (define-values (candidate candidate-class) (recompilation-candidate vm ctx))
  (if candidate
      (recompile-method! vm candidate-class candidate)
      (log-vm/jit/recompile-info "No recompilation candidate available?")))

(define (compile-method-proc compile-time-vm class method old-picmap)
  (define c (top-compilation compile-time-vm class method old-picmap))
  (define body-code (gen-jump-to-label c 0 '())) ;; imperative!
  (define pic-infos (reverse (compilation-result-pic-list-rev (compilation-state c))))
  (define pic-infos-exp (gen-lit* (compilation-result-litmap (compilation-state c)) pic-infos))
  (define stable? (equal? (if old-picmap (list->set (hash-keys old-picmap)) 'unknown)
                          (list->set (map pic-info-context pic-infos))))
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
                   method
                   (for/list [(pi (in-list ,pic-infos-exp))
                              (pic (in-list (list ,@(map pic-info-variable pic-infos))))]
                     (cons pi pic))
                   ,stable?)))
          cmi]
         [(vm k ,@(vector->list (compilation-argnames c)))
          (set! call-counter (+ call-counter 1))
          ;; TODO: aging of call-counter by right-shifting at most once every few seconds, or so
          (when (= call-counter 1000)
            (log-vm/jit/recompile-info "Method ~a is hot" ,(method-name method class))
            (recompile-something vm (k))
            ;; (set! call-counter 0)
            )
          (let ((outer-k k)
                (temporaries ,(gen-fresh-temps method)))
            ,(gen-label-definitions c body-code))])))
  (finish-compilation c compile-time-vm inner-code))

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

(define (lookup-method/cache vm class name-bytes)
  (define class-cache (hash-ref! (jit-VM-cache vm) class make-weak-hash))
  (hash-ref! class-cache
             name-bytes
             (lambda () (cached-method class name-bytes #f #f))))

(define (lookup-message/jit vm pic class selector)
  (let search-pic ((slot-index 0))
    (define this-class (pic@ pic slot-index 0))
    (if (eq? this-class class)
        (begin (pic-bump! pic slot-index)
               (or (unwrap-cached-method vm (pic@ pic slot-index 1))
                   (send-dnu class selector)))
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
                    (send-dnu class selector))))))))

(define ((send-dnu class selector) vm ctx . args)
  (define arguments (obj (VM-Array vm) (list->vector args)))
  (define dnu-name-bytes #"doesNotUnderstand:")
  (match (unwrap-cached-method vm (lookup-method/cache vm class dnu-name-bytes))
    [#f (error 'send-message* "Unhandled selector ~a at class ~a" selector class)]
    [dnu-method
     (log-vm-warning "DNU -- arguments ~a class ~a selector ~a" arguments class selector)
     (dnu-method vm ctx (slotAt arguments 0) (mkobj (VM-Array vm) selector arguments))]))

(define (block->thunk vm block args) ;; Expects a real bytecode block, not an ffiv one
  (lambda ()
    (define method (slotAt block 0))
    (define outer-args (vector->list (obj-slots (slotAt block 1))))
    (define temporaries (obj-slots (slotAt block 2)))
    (define argument-location (slotAt block 7))
    (define block-ip (slotAt block 9))
    (define f (compile-block-proc vm method outer-args temporaries argument-location block-ip))
    (apply f vm (outermost-k vm) args)))

(define (outermost-k vm)
  (case-lambda [() (VM-nil vm)]
               [(result) result]))

;;===========================================================================

(define-primitive vm [6 inner-ctx] ;; "new context execute"
  (when (not (zero? (slotAt inner-ctx 5))) (error 'execute "Cannot execute from nonempty stack"))
  (when (not (zero? (slotAt inner-ctx 4))) (error 'execute "Cannot execute from nonzero IP"))
  (define args (slotAt inner-ctx 1))
  (define f (compile-method-proc vm (obj-class* vm (slotAt args 0)) (slotAt inner-ctx 0) #f))
  (apply f vm (outermost-k vm) (vector->list (obj-slots args))))

(define-primitive vm [116]
  (let ((image-bytes (serialize-image vm)))
    (display-to-file image-bytes (jit-VM-image-filename vm) #:exists 'replace)))

;;===========================================================================

(pretty-print-columns 132)
(let* ((image-filename "SmallWorld/src/image")
       (vm (call-with-input-file image-filename
             (lambda (fh)
               (read-image fh jit-VM (list (make-weak-hasheq) image-filename))))))
  (boot-image vm
              (lambda (vm source)
                (define compiled-method
                  (unwrap-cached-method vm (lookup-method/cache vm (obj-class source) #"doIt")))
                (compiled-method vm (outermost-k vm) source))
              (current-command-line-arguments)))
