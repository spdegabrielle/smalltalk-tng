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

(define pic-entry-count 3)
(define (pic name-bytes send-ip)
  ;; pic-entry-count times three - one each for class, method, and
  ;; count - plus two, name-bytes and send-ip.
  (vector name-bytes send-ip
          #f #f 0
          #f #f 0
          #f #f 0))

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

(struct pic-info (name-bytes variable ip))
(struct compilation-result (litmap [pic-list-rev #:mutable]))
(struct compilation (depth vm receiver-class method argnames labels state))

(struct compiled-method-info (bytecode-method pics))

(define (build-jit-context vm previous-context args method ip stack-top temporaries stack)
  (define max-stack (slotAt method 3))
  (mkobj (VM-Context vm)
         method
         (obj (VM-Array vm) args)
         (obj (VM-Array vm) temporaries)
         (obj (VM-Array vm) (vector-append stack (make-vector (- max-stack (vector-length stack))
                                                              (VM-nil vm))))
         ip
         stack-top
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

(define (compilation* depth compile-time-vm receiver-class method state)
  (define selector (slotAt method 0))
  (define arity (selector-string-arity (bv->string selector)))
  (define literals (slotAt method 2))

  (log-vm/jit/code-info
   "Compiling ~v defined in ~v, to be run in ~v (depth ~a), arity ~a, literals ~a, bytecode ~a, text:\n----\n~a\n----"
   (bv->string selector)
   (slotAt method 5)
   receiver-class
   depth
   arity
   literals
   (bytes->hex-string (bv-bytes (slotAt method 1)))
   (bv->string (slotAt method 6)))

  (define litmap (compilation-result-litmap state))
  (for [(lit (obj-slots literals))] (gen-lit* litmap lit))

  (define argnames (for/vector [(i arity)] (if (zero? i) 'self (mksym "arg~a" (- i 1)))))
  (compilation depth
               compile-time-vm
               receiver-class
               method
               argnames
               (make-hash)
               state))

(define (top-compilation vm receiver-class method)
  (compilation* 0 vm receiver-class method (compilation-result (make-hasheq) '())))

(define (inline-compilation c method)
  (match-define (compilation depth vm receiver-class _method _argnames _labels state) c)
  (compilation* (+ depth 1) vm receiver-class method state))

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
                      ,(length stack)
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

(define (gen-send c send-ip class-exp name-bytes selector-exp k-exp arg-exps)
  (define old-pics (compilation-result-pic-list-rev (compilation-state c)))
  (define pic-index (length old-pics))
  (define m (mksym "pic~a" pic-index))
  (define pi (pic-info name-bytes m send-ip))
  (set-compilation-result-pic-list-rev! (compilation-state c) (cons pi old-pics))
  (match class-exp
    [`(obj-class* vm self) #:when (< (compilation-depth c) 2) ;; self send
     (define receiver-class (compilation-receiver-class c))
     (define method (lookup-method (compilation-vm c) receiver-class name-bytes))
     (define defining-class (slotAt method 5))
     (log-vm/jit/code-debug "Self-send of ~a to class ~a" name-bytes receiver-class)
     (define ic (inline-compilation c method))
     (define body-code (gen-jump-to-label ic 0 '()))
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
     inner-code]
    [_
     `((lookup-message/jit vm ,m ,class-exp ,selector-exp) vm ,k-exp ,@arg-exps)]))

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
  (define litmap-list (hash->list (compilation-result-litmap (compilation-state c))))
  (define code
    `(lambda (method super NIL TRUE FALSE ARRAY BLOCK ,@(map cdr litmap-list))
       ,@(for/list [(pi (reverse (compilation-result-pic-list-rev (compilation-state c))))]
           `(define ,(pic-info-variable pi)
              (pic ,(pic-info-name-bytes pi)
                   ,(pic-info-ip pi))))
       ,inner-code))

  (log-vm/jit/code-debug "Resulting code:\n~a" (pretty-format code))
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
  (define c (top-compilation compile-time-vm class method))
  (define body-code (gen-block c argument-location initial-ip)) ;; imperative!
  (define inner-code
    `(lambda (temporaries ,@(vector->list (compilation-argnames c)))
       (let ((outer-k (outermost-k vm)))
         ,(gen-label-definitions c body-code))))
  (apply (finish-compilation c compile-time-vm inner-code)
         actual-temporaries
         outer-args))

(define (dump-stack vm ctx)
  (when (not (eq? (VM-nil vm) ctx))
    (define method (slotAt ctx 0))
    (define selector (slotAt method 0))
    (define receiver (slotAt (slotAt ctx 1) 0))
    (define receiver-class (obj-class* vm receiver))
    (define next-ctx (slotAt ctx 6))
    (log-vm/jit/recompile-info "  ~a >> ~a"
                               (bv->string (slotAt receiver-class 0))
                               (bv->string selector))
    (define compiled-method (lookup-method/cache vm receiver-class (bv-bytes selector)))
    (when compiled-method
      (match-define (compiled-method-info bytecode-method pics) (compiled-method))
      (log-vm/jit/recompile-info "    has ~a bytes of bytecode"
                                 (bytes-length (bv-bytes (slotAt bytecode-method 1))))
      (for [(pic pics)]
        (define (pic-has-any-calls? pic)
          (or (positive? (vector-ref pic 4))
              (positive? (vector-ref pic 7))
              (positive? (vector-ref pic 10))))
        (when (pic-has-any-calls? pic)
          (log-vm/jit/recompile-info "      ~a @~a ~a"
                                     (vector-ref pic 0)
                                     (vector-ref pic 1)
                                     (for/list [(i (in-range 2 (vector-length pic) 3))]
                                       (define c (vector-ref pic i))
                                       (if c
                                           (match ((vector-ref pic (+ i 1)))
                                             [(compiled-method-info bcm _pics)
                                              (list (bv->string (slotAt c 0))
                                                    (vector-ref pic (+ i 2))
                                                    (bytes-length (bv-bytes (slotAt bcm 1))))])
                                           '-))))))
    (dump-stack vm next-ctx)))

(define (compile-method-proc compile-time-vm class method)
  (define c (top-compilation compile-time-vm class method))
  (define body-code (gen-jump-to-label c 0 '())) ;; imperative!
  (define inner-code
    `(let ((call-counter 0))
       (case-lambda
         [()
          (compiled-method-info
           method
           (list
            ,@(let ((pic-count (length (compilation-result-pic-list-rev (compilation-state c)))))
                (for/list [(n (in-range pic-count))] (mksym "pic~a" n)))))]
         [(vm k ,@(vector->list (compilation-argnames c)))
          (set! call-counter (+ call-counter 1))
          (when (= call-counter 1000)
            (log-vm/jit/recompile-info "Method ~a of class ~a is hot"
                                       ,(bv->string (slotAt method 0))
                                       ,(bv->string (slotAt class 0)))
            (dump-stack vm (k))
            ;; (set! call-counter 0)
            )
          (let ((outer-k k)
                (temporaries ,(gen-fresh-temps method)))
            ,(gen-label-definitions c body-code))])))
  (finish-compilation c compile-time-vm inner-code))

(define (lookup-method/cache vm class name-bytes)
  (define class-cache (hash-ref! (jit-VM-cache vm) class make-weak-hash))
  (hash-ref! class-cache
             name-bytes
             (lambda ()
               (define m (lookup-method vm class name-bytes))
               (and m (compile-method-proc vm class m)))))

(define (lookup-message/jit vm pic class selector)
  (define reserved 2)
  (define (@ i o) (vector-ref pic (+ reserved o (* i 3))))
  (define (@! i o v) (vector-set! pic (+ reserved o (* i 3)) v))
  (let search-pic ((slot-index 0))
    (define this-class (@ slot-index 0))
    (if (eq? this-class class)
        (begin (@! slot-index 2 (+ 1 (@ slot-index 2)))
               (@ slot-index 1))
        (let* ((next-slot-index (+ slot-index 1))
               (more-slots-to-check? (and this-class (< next-slot-index pic-entry-count))))
          (if more-slots-to-check?
              (search-pic next-slot-index)
              (let ((method (lookup-method/cache vm class (bv-bytes selector))))
                (if (not method)
                    (lambda (vm ctx . args)
                      (send-dnu vm ctx (obj (VM-Array vm) (list->vector args)) class selector))
                    (let ((slot-empty? (not this-class)))
                      (when slot-empty?
                        (@! slot-index 0 class)
                        (@! slot-index 1 method)
                        (@! slot-index 2 1))
                      method))))))))

(define (send-dnu vm ctx arguments class selector)
  (define dnu-name-bytes #"doesNotUnderstand:")
  (match (lookup-method/cache vm class dnu-name-bytes)
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
  (define f (compile-method-proc vm (obj-class* vm (slotAt args 0)) (slotAt inner-ctx 0)))
  (apply f vm (outermost-k vm) (vector->list (obj-slots args))))

(define-primitive vm [116]
  (let ((image-bytes (serialize-image vm)))
    (display-to-file image-bytes (jit-VM-image-filename vm) #:exists 'replace)))

;;===========================================================================

(let* ((image-filename "SmallWorld/src/image")
       (vm (call-with-input-file image-filename
             (lambda (fh)
               (read-image fh jit-VM (list (make-weak-hasheq) image-filename))))))
  (boot-image vm
              (lambda (vm source)
                ((lookup-method/cache vm (obj-class source) #"doIt") vm (outermost-k vm) source))
              (current-command-line-arguments)))
