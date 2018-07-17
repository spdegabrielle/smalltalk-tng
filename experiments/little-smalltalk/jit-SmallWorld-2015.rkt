#lang racket/gui
;; Loader for images (version 1 format) from Russell Allen's 2015
;; variant of SmallWorld, a Tim Budd-authored Little Smalltalk
;; descendant.

(require racket/struct)
(require racket/bytes)
(require (only-in sha bytes->hex-string))
(require "oneshot.rkt")

(define-logger vm)
(define-logger vm/gui)
(define-logger vm/jit)

(struct obj ([class #:mutable] slots)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer (lambda (o) (format "obj:~a" (obj-class-name o)))
                                     (lambda (o)
                                       (match (obj-class-name o)
                                         [#"Array" (list (vector->list (obj-slots o)))]
                                         [#"Class" (list (slotAt o 0))]
                                         [_ '()]))))])

(struct bv obj (bytes)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer (lambda (o) (format "bv:~a" (obj-class-name o)))
                                     (lambda (o) (list (bv-bytes o)))))])

(struct ffiv obj (value)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer (lambda (o) (format "ffiv:~a" (obj-class-name o)))
                                     (lambda (o) (list (ffiv-value o)))))])

(define-match-expander unbv
  (syntax-rules () [(_ bytes-pat) (bv _ _ bytes-pat)]))
(define-match-expander unbv*
  (syntax-rules () [(_ this-pat bytes-pat) (and this-pat (bv _ _ bytes-pat))]))
(define-match-expander unstr
  (syntax-rules () [(_ str-pat) (bv _ _ (app bytes->string/utf-8 str-pat))]))
(define-match-expander unffiv
  (syntax-rules () [(_ val-pat) (ffiv _ _ val-pat)]))
(define-match-expander unffiv*
  (syntax-rules () [(_ this-pat val-pat) (and this-pat (ffiv _ _ val-pat))]))

(define (bv->string b)
  (bytes->string/utf-8 (bv-bytes b)))

(define (obj-class-name o)
  (define c (obj-class o))
  (if (and (positive? (slotCount c))
           (bv? (slotAt c 0)))
      (bv-bytes (slotAt c 0))
      #"???"))

(struct VM (nil true false Array Block Context Integer cache image-filename))

(define (read-image image-filename fh)

  (define (next-int #:signed? [signed? #t] #:eof-ok? [eof-ok? #f])
    (define bs (read-bytes 4 fh))
    (if (eof-object? bs)
        (if eof-ok? bs (error 'read-image "Early EOF"))
        (integer-bytes->integer bs signed? #t)))

  (let ((image-version (next-int))
        (expected-version 1))
    (when (not (= image-version expected-version))
      (error 'read-image "Wrong image version: got ~a, expected ~a"
             image-version
             expected-version)))

  (define object-table
    (let loop ((acc '()))
      (define (emit x) (loop (cons x acc)))
      (match (next-int #:eof-ok? #t)
        [(? eof-object?) (list->vector (reverse acc))]
        [obj-length
         (define type-code (next-int))
         (define class-index (next-int))
         (define slot-count (next-int))
         (match type-code
           [0 ;; SmallInt
            (when (not (= obj-length 5))
              (error 'read-image "Strange SmallInt obj-length: ~a" obj-length))
            (when (not (zero? slot-count))
              (error 'read-image "Strange SmallInt with ~a slots" slot-count))
            (emit (next-int))]
           [1 ;; SmallByteArray
            (define byte-count (- obj-length slot-count 4))
            (emit (bv class-index
                      (for/vector [(i slot-count)] (next-int))
                      (read-bytes byte-count fh)))]
           [2 ;; SmallObject
            (emit (obj class-index
                       (for/vector [(i slot-count)] (next-int))))])])))

  (for [(x object-table)]
    (when (obj? x)
      (set-obj-class! x (vector-ref object-table (obj-class x)))
      (for [(i (vector-length (obj-slots x)))]
        (vector-set! (obj-slots x) i (vector-ref object-table (vector-ref (obj-slots x) i))))))

  (VM (vector-ref object-table 0)
      (vector-ref object-table 1)
      (vector-ref object-table 2)
      (vector-ref object-table 3)
      (vector-ref object-table 4)
      (vector-ref object-table 5)
      (vector-ref object-table 6)
      (make-weak-hasheq)
      image-filename))

(define (serialize-image vm)
  (define indices (make-hasheq))
  (define output-rev '())
  (define worklist-rev '())
  (define next-index 0)

  (define (push-bytes! item) (set! output-rev (cons item output-rev)))
  (define (push-int! n) (push-bytes! (integer->integer-bytes n 4 #t #t)))

  (define (object->index o)
    (if (ffiv? o)
        (object->index (VM-nil vm))
        (hash-ref! indices o (lambda ()
                               (begin0 next-index
                                 (set! next-index (+ next-index 1))
                                 (set! worklist-rev (cons o worklist-rev)))))))

  (push-int! 1) ;; version number
  (object->index (VM-nil vm))
  (object->index (VM-true vm))
  (object->index (VM-false vm))
  (object->index (VM-Array vm))
  (object->index (VM-Block vm))
  (object->index (VM-Context vm))
  (object->index (VM-Integer vm))
  (for [(i 10)] (object->index i))

  (let loop ()
    (define worklist (reverse worklist-rev))
    (set! worklist-rev '())
    (when (pair? worklist)
      (for [(o worklist)]
        (match o
          [(? number?)
           (push-int! 5)
           (push-int! 0)
           (push-int! (object->index (VM-Integer vm)))
           (push-int! 0)
           (push-int! o)]
          [(bv class slots bytes)
           (push-int! (+ (bytes-length bytes) (vector-length slots) 4)) ;; weird
           (push-int! 1)
           (push-int! (object->index class))
           (push-int! (vector-length slots))
           (for [(s slots)] (push-int! (object->index s)))
           (push-bytes! bytes)]
          [(obj class slots)
           (push-int! (+ (vector-length slots) 4)) ;; weird
           (push-int! 2)
           (push-int! (object->index class))
           (push-int! (vector-length slots))
           (for [(s slots)] (push-int! (object->index s)))]))
      (loop)))

  (bytes-append* (reverse output-rev)))

(define (slotCount o) (vector-length (obj-slots o)))
(define (slotAt o i) (vector-ref (obj-slots o) i))
(define (slotAtPut o i v) (vector-set! (obj-slots o) i v))

(define (search-class-method-dictionary c name-bytes)
  (define methods (slotAt c 2))
  (for/first [(m (obj-slots methods))
              #:when (equal? name-bytes (bv-bytes (slotAt m 0)))]
    m))

(define (mkobj cls . fields)
  (obj cls (list->vector fields)))

(define (mkbv cls bs . fields)
  (bv cls (list->vector fields) bs))

(define (mkffiv cls value)
  (ffiv cls '#() value))

(define (mkarray vm count [init (VM-nil vm)])
  (obj (VM-Array vm) (make-vector count init)))

(define (build-context vm previous-context args method)
  (define temp-count (slotAt method 4))
  (define max-stack (slotAt method 3))
  (mkobj (VM-Context vm)
         method
         args
         (mkarray vm temp-count)
         (mkarray vm max-stack)
         0 ;; IP
         0 ;; stack top
         previous-context))

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

(define (clone-array a [start 0] [count (- (slotCount a) start)])
  (define b (obj (obj-class a) (make-vector count)))
  (for [(i (in-range count))]
    (slotAtPut b i (slotAt a (+ i start))))
  b)

(define (boolean->obj vm b)
  (if b (VM-true vm) (VM-false vm)))

(define (selector-string-arity str)
  (define colon-count (for/sum [(c str)] (if (eqv? c #\:) 1 0)))
  (cond [(positive? colon-count) (+ colon-count 1)]
        [(char-alphabetic? (string-ref str 0)) 1]
        [else 2])) ;; assume binary operator

(define-namespace-anchor ns-anchor)
(define ns (namespace-anchor->namespace ns-anchor))

(define (compile-native-proc compile-time-vm method)
  (define selector (slotAt method 0))
  (define arity (selector-string-arity (bv->string selector)))
  (define bytecode (bv-bytes (slotAt method 1)))
  (define literals (slotAt method 2))
  (define max-stack (slotAt method 3))
  (define temp-count (slotAt method 4))
  (define defining-class (slotAt method 5))
  (define method-source (slotAt method 6))

  (log-vm/jit-info
   "Compiling ~v defined in ~v, arity ~a, literals ~a, bytecode ~a, text:\n----\n~a\n----"
   (bv->string selector)
   defining-class
   arity
   literals
   (bytes->hex-string bytecode)
   (bv->string method-source))

  (define (mksym fmt . args) (string->symbol (apply format fmt args)))

  (define litnames (for/vector [(i (slotCount literals))]
                     (define lit (slotAt literals i))
                     (if (bv? lit)
                         (mksym "lit~a-~a" i (bv->string lit))
                         (mksym "lit~a" i))))
  (define tmpnames (for/vector [(i temp-count)] (mksym "tmp~a" i)))
  (define argnames (for/vector [(i arity)] (if (zero? i) 'self (mksym "arg~a" (- i 1)))))

  (define (build-jit-context-exp ip stack)
    `(build-jit-context vm
                        (k)
                        (vector ,@(vector->list argnames))
                        method
                        ,ip
                        ,(length stack)
                        (vector ,@(vector->list tmpnames))
                        (vector ,@(reverse stack))))

  (define-syntax let@
    (syntax-rules ()
      [(_ [n n-code-exp] body-code-exp)
       (let@ [n (gensym 'n) n-code-exp] body-code-exp)]
      [(_ [n n-exp n-code-exp] body-code-exp)
       (let ((n (gensym n-exp)))
         `(let ((,n ,n-code-exp))
            ,body-code-exp))]))

  (define labels (make-hash))

  (define (jump-to-label ip stack)
    (when (not (hash-has-key? labels ip))
      (hash-set! labels ip 'placeholder)
      (define actual-label
        (let ((newstack (for/list [(i (length stack))] (mksym "stack~a" i))))
          `(lambda (k ,@newstack) ,(translate ip newstack))))
      (hash-set! labels ip actual-label))
    `(,(mksym "label~a" ip) k ,@stack))

  (define mic-count 0)
  (define (next-mic!)
    (begin0 mic-count
      (set! mic-count (+ mic-count 1))))

  (define (gen-send-k ip stack)
    (define result (gensym 'result))
    `(case-lambda [() ,(build-jit-context-exp ip stack)]
                  [(,result) ,(translate ip (cons result stack))]))

  (define (gen-send class-exp selector-exp k-exp arg-exps)
    (define mic-index (next-mic!))
    (define mc (mksym "mic~a-class" mic-index))
    (define mm (mksym "mic~a-method" mic-index))
    `((lookup-message/jit vm ,mc ,mm ,class-exp ,selector-exp) vm ,k-exp ,@arg-exps))

  (define (translate ip stack)
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
      [2 (translate ip (cons (vector-ref argnames arg) stack))]
      [3 (let@ [n (mksym "tmp~a_" arg) (vector-ref tmpnames arg)]
               (translate ip (cons n stack)))]
      [4 (translate ip (cons (vector-ref litnames arg) stack))]
      [5 (match arg
           [(or 0 1 2 3 4 5 6 7 8 9) (translate ip (cons arg stack))]
           [10 (translate ip (cons `NIL stack))]
           [11 (translate ip (cons `TRUE stack))]
           [12 (translate ip (cons `FALSE stack))])]
      [6 `(begin (slotAtPut self ,arg ,(car stack)) ,(translate ip stack))]
      [7 `(begin (set! ,(vector-ref tmpnames arg) ,(car stack)) ,(translate ip stack))]
      [8 (let* ((arg-count arg)
                (args (reverse (take stack arg-count)))
                (stack (drop stack arg-count)))
           (define-values (selector-literal-index class-exp)
             (match/values (decode!)
               [(9 selector-literal-index)
                (values selector-literal-index `(obj-class* vm ,(car args)))]
               [(15 11)
                (values (next-byte!) `super)]))
           (define k (gen-send-k ip stack))
           (gen-send class-exp (vector-ref litnames selector-literal-index) k args))]

      ;; [9 (let ((args (car stack))
      ;;          (result (gensym 'result)))
      ;;      (log-vm/jit-debug "send of ~a" (slotAt literals arg))
      ;;      `(send-message vm
      ;;                     (case-lambda
      ;;                       [() ,(build-jit-context-exp ip (cdr stack))]
      ;;                       [(,result) ,(translate ip (cons result (cdr stack)))])
      ;;                     (obj ARRAY (list->vector ,args))
      ;;                     ,(vector-ref litnames arg)))]

      [10 (match arg
            [0 (let@ [isNil `(boolean->obj vm (eq? NIL ,(car stack)))]
                     (translate ip (cons isNil (cdr stack))))]
            [1 (let@ [notNil `(boolean->obj vm (not (eq? NIL ,(car stack))))]
                     (translate ip (cons notNil (cdr stack))))])]
      [11 (match stack
            [(list* j i stack)
             (let@ [binop-k (gen-send-k ip stack)]
                   `(if (and (number? ,i) (number? ,j))
                        ,(match arg
                           [0 `(,binop-k (boolean->obj vm (< ,i ,j)))]
                           [1 `(,binop-k (boolean->obj vm (<= ,i ,j)))]
                           [2 `(,binop-k (+ ,i ,j))])
                        ,(gen-send `(obj-class* vm ,i)
                                   `(mkbv NIL ,(match arg [0 #"<"] [1 #"<="] [2 #"+"]))
                                   binop-k
                                   (list i j))))])]
      [12 (let ((target (next-byte!)))
            (let@ [block `(mkffiv BLOCK
                                  (lambda (_vm k . block-arguments)
                                    ,(let loop ((i arg))
                                       (if (>= i temp-count)
                                           `(void)
                                           `(when (pair? block-arguments)
                                              (set! ,(vector-ref tmpnames i) (car block-arguments))
                                              (let ((block-arguments (cdr block-arguments)))
                                                ,(loop (+ i 1))))))
                                    ,(translate ip '())))]
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
                                 ;; ^ reason being the image BUGGILY relies on primitive 8
                                 ;; immediately returning to the surrounding context!!
                                 ,@(reverse (take stack argc)))]
                    [(obj (== BLOCK) _)
                     (k ((block->thunk vm ,block (list ,@(reverse (take stack argc))))))]))]
            [34 'NIL]
            [35 (let@ [ctxref (build-jit-context-exp ip stack)]
                      (translate ip (cons ctxref stack)))]
            [36 (let@ [arr `(mkobj ARRAY ,@(reverse (take stack arg)))]
                      (translate ip (cons arr (drop stack arg))))]
            [_ (let ((generator (hash-ref *primitive-code-snippets*
                                          primitive-number
                                          (lambda () (error 'compile-native-proc
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
            [6 (jump-to-label (next-byte!) stack)]
            [7 (let ((target (next-byte!)))
                 (log-vm/jit-debug "if ~a true jump to ~a, else continue at ~a" (car stack) target ip)
                 `(if (eq? ,(car stack) TRUE)
                      ,(jump-to-label target (cdr stack))
                      ,(jump-to-label ip (cdr stack))))]
            [8 (let ((target (next-byte!)))
                 (log-vm/jit-debug "if ~a false jump to ~a, else continue at ~a" (car stack) target ip)
                 `(if (eq? ,(car stack) FALSE)
                      ,(jump-to-label target (cdr stack))
                      ,(jump-to-label ip (cdr stack))))]
            ;; [11 (let ((args (car stack))
            ;;           (result (gensym 'result))
            ;;           (selector-literal (next-byte!)))
            ;;       `(send-message* vm
            ;;                       (case-lambda
            ;;                         [() ,(build-jit-context-exp ip (cdr stack))]
            ;;                         [(,result) ,(translate ip (cons result (cdr stack)))])
            ;;                       (obj ARRAY (list->vector ,args))
            ;;                       super
            ;;                       ,(vector-ref litnames selector-literal)))]
            [_ (error 'compile-native-proc "Unhandled do-special case ~v" arg)])]
      [_ (error 'compile-native-proc "Method ~v - unhandled opcode ~v, arg ~v"
                selector
                opcode
                arg)]))

  (define code
    (let ((inner (jump-to-label 0 '())))
      `(lambda (method super NIL TRUE FALSE ARRAY BLOCK ,@(vector->list litnames))
         ,@(for/list [(i mic-count)] `(define ,(mksym "mic~a-class" i) (box NIL)))
         ,@(for/list [(i mic-count)] `(define ,(mksym "mic~a-method" i) (box NIL)))
         (lambda (vm k ,@(vector->list argnames))
           (let ((outer-k k)
                 ,@(for/list [(t tmpnames)] `(,t NIL)))
             (letrec (,@(for/list [((ip label) (in-hash labels))]
                          `(,(mksym "label~a" ip)
                            ,label)))
               ,inner))))))
  (log-vm/jit-info "Resulting code:\n~a" (pretty-format code))
  (define final-proc
    (apply (eval code ns)
           method
           (slotAt defining-class 1) ;; defining class's superclass
           (VM-nil compile-time-vm) ;; assuming this VM is the one that will be used at call time!
           (VM-true compile-time-vm)
           (VM-false compile-time-vm)
           (VM-Array compile-time-vm)
           (VM-Block compile-time-vm)
           (vector->list (obj-slots literals))))
  (log-vm/jit-info "Final proc: ~a" final-proc)
  final-proc)

(define (install-native-proc! vm class selector native-proc)
  (define name-bytes (bv-bytes selector))
  (define class-cache (hash-ref! (VM-cache vm) class make-weak-hash))
  (hash-set! class-cache name-bytes native-proc)
  native-proc)

(define (lookup-method/cache vm class selector)
  (define name-bytes (bv-bytes selector))
  (define class-cache (hash-ref! (VM-cache vm) class make-weak-hash))
  (hash-ref! class-cache name-bytes (lambda () (lookup-method vm class selector))))

(define (lookup-method vm class selector)
  (define name-bytes (bv-bytes selector))
  (let search ((class class))
    (and (not (eq? class (VM-nil vm)))
         (or (search-class-method-dictionary class name-bytes)
             (search (slotAt class 1))))))

(define (store-registers! ctx ip stack-top)
  (slotAtPut ctx 4 ip)
  (slotAtPut ctx 5 stack-top))

;; (define (lookup-message/jit vm mic-class mic-method class selector)
;;   (when (not (eq? (unbox mic-class) class))
;;     (set-box! mic-class class)
;;     (set-box! mic-method #f))
;;   (when (not (unbox mic-method))
;;     (set-box! mic-method (lookup-method/cache vm class selector))
;;     (when (not (procedure? (unbox mic-method)))
;;       (set-box! mic-method (install-native-proc! vm
;;                                                  class
;;                                                  selector
;;                                                  (compile-native-proc vm (unbox mic-method))))))
;;   (or (unbox mic-method)
;;       (lambda (vm ctx . args)
;;         (send-dnu vm ctx (obj (VM-Array vm) (list->vector args)) class selector))))

(define (lookup-message/jit vm mic-class mic-method class selector)
  (define method (unbox mic-method))
  (when (or (not (eq? (unbox mic-class) class))
            (not method))
    (set-box! mic-class class)
    (set! method (lookup-method/cache vm class selector))
    (when (and method (not (procedure? method)))
      (set! method (install-native-proc! vm
                                         class
                                         selector
                                         (compile-native-proc vm method))))
    (set-box! mic-method method))
  (or method
      (lambda (vm ctx . args)
        (send-dnu vm ctx (obj (VM-Array vm) (list->vector args)) class selector))))

(define (send-dnu vm ctx arguments class selector)
  (define dnu-selector (mkbv (obj-class selector) #"doesNotUnderstand:"))
  (match (lookup-method/cache vm class dnu-selector)
    [#f (error 'send-message* "Unhandled selector ~a at class ~a" selector class)]
    [dnu-method
     (log-vm-warning "DNU -- arguments ~a class ~a selector ~a" arguments class selector)
     (apply-method class
                   dnu-selector
                   dnu-method
                   vm
                   ctx
                   (list (slotAt arguments 0)
                         (mkobj (VM-Array vm)
                                selector
                                (clone-array arguments))))]))

(define (send-message* vm ctx arguments class selector)
  (match (lookup-method/cache vm class selector)
    [#f (send-dnu vm ctx arguments class selector)]
    [new-method (apply-method class
                              selector
                              new-method
                              vm
                              ctx
                              (vector->list (obj-slots arguments)))]))

(define (apply-method class selector method vm ctx arglist)
  (define native-proc
    (if (procedure? method)
        method
        (install-native-proc! vm class selector (compile-native-proc vm method))))
  (apply native-proc
         vm
         (if (procedure? ctx)
             ctx
             (case-lambda
               [() ctx]
               [(result) (resume-context vm ctx result)]))
         arglist))

(define (obj-class* vm o)
  (if (number? o)
      (VM-Integer vm)
      (obj-class o)))

(define (send-message vm ctx arguments selector)
  (log-vm-debug "sending: ~a ~a" selector arguments)
  (send-message* vm ctx arguments (obj-class* vm (slotAt arguments 0)) selector))

(define (block->thunk vm block args) ;; Expects a real bytecode block, not an ffiv one
  (let ((ctx (clone-array block)))
    (define argument-location (slotAt ctx 7))
    (for [(i (in-naturals argument-location)) (arg (in-list args))]
      (slotAtPut (slotAt ctx 2) i arg))
    (slotAtPut ctx 3 (mkarray vm (slotCount (slotAt ctx 3))))
    (slotAtPut ctx 4 (slotAt ctx 9)) ;; reset IP to correct block offset
    (slotAtPut ctx 5 0) ;; zero stack-top
    (slotAtPut ctx 6 (VM-nil vm)) ;; no previous context
    (lambda () (execute vm ctx))))

(define (block-callback vm block)
  ;; Runs block in a new thread
  (lambda args
    (match block
      [(unffiv block-proc)
       (thread (lambda () (apply block-proc
                                 vm
                                 (case-lambda [() (VM-nil vm)] [(result) (void)])
                                 args)))]
      [_
       (thread (block->thunk vm block args))])))

(define smalltalk-frame%
  (class frame%
    (field [close-handler void])
    (define/public (set-close-handler new-handler)
      (set! close-handler new-handler))
    (define/augment (on-close)
      (close-handler this))
    (super-new)))

(define (resume-context vm ctx result)
  (if (eq? (VM-nil vm) ctx)
      result
      (let ((stack-top (slotAt ctx 5)))
        (slotAtPut (slotAt ctx 3) stack-top result)
        (slotAtPut ctx 5 (+ stack-top 1))
        (log-vm-debug "resuming: ~a" result)
        (execute vm ctx))))

(define (execute vm ctx)
  (define method (slotAt ctx 0))
  (define arguments (slotAt ctx 1))
  (define temporaries (slotAt ctx 2))
  (define stack (slotAt ctx 3))
  (define ip (slotAt ctx 4))
  (define stack-top (slotAt ctx 5))
  (define previous-ctx (slotAt ctx 6))

  (define receiver (slotAt arguments 0))

  (define bytecode (bv-bytes (slotAt method 1)))
  (define literals (slotAt method 2))

  (log-vm-info "Interpreter bytecode, ctx slotcount ~a, method name ~a: ~a"
               (slotCount ctx)
               (bv->string (slotAt method 0))
               (bytes->hex-string bytecode))

  (define (push! v)
    (slotAtPut stack stack-top v)
    (set! stack-top (+ stack-top 1)))
  (define (pop!)
    (set! stack-top (- stack-top 1))
    (slotAt stack stack-top))
  (define (peek)
    (slotAt stack (- stack-top 1)))

  (define (pop-multiple! count)
    (set! stack-top (- stack-top count))
    (clone-array stack stack-top count))

  (define (continue-from next-ip)
    (set! ip next-ip)
    (interpret))

  (define (push-and-go next-ip v)
    (push! v)
    (continue-from next-ip))

  (define (push-and-continue v)
    (push! v)
    (interpret))

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

  (define (interpret)
    (define-values (high low) (decode!))
    (log-vm-debug "> ~a ~a ~a" high low (vector-copy (obj-slots stack) 0 stack-top))
    (match high
      [1 (push-and-continue (slotAt receiver low))] ;; PushInstance
      [2 (push-and-continue (slotAt arguments low))] ;; PushArgument
      [3 (push-and-continue (slotAt temporaries low))] ;; PushTemporary
      [4 (push-and-continue (slotAt literals low))] ;; PushLiteral
      [5 (match low
           [(or 0 1 2 3 4 5 6 7 8 9) (push-and-continue low)]
           [10 (push-and-continue (VM-nil vm))]
           [11 (push-and-continue (VM-true vm))]
           [12 (push-and-continue (VM-false vm))])]
      [6 (slotAtPut receiver low (peek)) (interpret)] ;; AssignInstance
      [7 (slotAtPut temporaries low (peek)) (interpret)] ;; AssignTemporary
      [8 (push-and-continue (pop-multiple! low))] ;; MarkArguments
      [9 ;; SendMessage
       (define new-arguments (pop!))
       (store-registers! ctx ip stack-top)
       (send-message vm ctx new-arguments (slotAt literals low))]

      [10 (match low
            [0 (push-and-continue (boolean->obj vm (eq? (VM-nil vm) (pop!))))] ;; isNil
            [1 (push-and-continue (boolean->obj vm (not (eq? (VM-nil vm) (pop!)))))])] ;; notNil

      [11 ;; SendBinary
       (define j (pop!))
       (define i (pop!))
       (if (and (number? i) (number? j))
           (match low
             [0 (push-and-continue (boolean->obj vm (< i j)))]
             [1 (push-and-continue (boolean->obj vm (<= i j)))]
             [2 (push-and-continue (+ i j))]) ;; TODO: overflow to bignum arithmetic
           (let ((new-arguments (mkobj (VM-Array vm) i j))
                 (selector (match low
                             [0 (mkbv (VM-nil vm) #"<")]
                             [1 (mkbv (VM-nil vm) #"<=")]
                             [2 (mkbv (VM-nil vm) #"+")])))
             (store-registers! ctx ip stack-top)
             (send-message vm ctx new-arguments selector)))]

      [12 ;; PushBlock
       (define target (next-byte!))
       (log-vm-debug "pushblock; temporaries = ~a" temporaries)
       (push-and-go target
        (mkobj (VM-Block vm) method arguments temporaries stack ip 0 previous-ctx low ctx ip))]

      [13 ;; Primitive; low = arg count; next byte = primitive number
       (define primitive-number (next-byte!))
       (log-vm-debug "primitive ~a (arg count = ~a)" primitive-number low)
       (match primitive-number
         [8 ;; block invocation
          (define block (pop!))
          (define argument-location (slotAt block 7))
          (define argument-count (- low 1)) ;; one of the primitive args is the block itself
          (for [(i argument-count)]
            (slotAtPut (slotAt block 2)
                       (+ argument-location i)
                       (slotAt stack (+ (- stack-top argument-count) i))))
          (set! stack-top (- stack-top argument-count))
          (store-registers! ctx ip stack-top)
          (execute vm (mkobj (VM-Context vm)
                             (slotAt block 0)
                             (slotAt block 1)
                             (slotAt block 2)
                             (mkarray vm (slotCount (slotAt block 3))) ;; new stack (!)
                             (slotAt block 9) ;; starting IP
                             0 ;; stack top
                             (slotAt ctx 6) ;; previous context
                             (slotAt block 7)
                             (slotAt block 8)
                             (slotAt block 9)))]
         [34 (VM-nil vm)] ;; "thread kill"
         [35 (push-and-continue ctx)]
         [36 (push-and-continue (pop-multiple! low))] ;; "fast array creation"

         [_ (define args (pop-multiple! low))
            (push-and-continue (perform-primitive vm primitive-number args))])]

      [14 (push-and-continue (slotAt (obj-class* vm receiver) (+ low 5)))] ;; PushClassVariable
      [15 ;; Do Special
       (match low
         [1 (resume-context vm previous-ctx receiver)]
         [2 (resume-context vm previous-ctx (pop!))]
         [3 (resume-context vm (slotAt (slotAt ctx 8) 6) (pop!))]
         [4 (push-and-continue (peek))]
         [5 (pop!) (interpret)]
         [6 (continue-from (next-byte!))]
         [7 ;; branch if true
          (define target (next-byte!))
          (if (eq? (pop!) (VM-true vm))
              (continue-from target)
              (interpret))]
         [8 ;; branch if false
          (define target (next-byte!))
          (if (eq? (pop!) (VM-false vm))
              (continue-from target)
              (interpret))]
         [11 ;; send to super
          (define selector (slotAt literals (next-byte!)))
          (define new-arguments (pop!))
          (define defining-class (slotAt method 5)) ;; method's defining class
          (define super (slotAt defining-class 1)) ;; defining class's superclass
          (store-registers! ctx ip stack-top)
          (send-message* vm ctx new-arguments super selector)])]))

  (interpret))

(define *primitive-handlers* (make-hash))
(define *primitive-code-snippets* (make-hash))

(define-syntax-rule (define-primitive vm [n arg-pat ...] body ...)
  (begin (hash-set! *primitive-handlers*
                    n
                    (lambda (vm args)
                      (match (obj-slots args) [(vector arg-pat ...) (let () body ...)])))
         (hash-set! *primitive-code-snippets*
                    n
                    (lambda (vm-exp arg-exps)
                      `(match* [,vm-exp ,@arg-exps]
                         [[vm arg-pat ...] (let () body ...)])))))

(define (perform-primitive vm primitive-number args)
  ((hash-ref *primitive-handlers*
             primitive-number
             (lambda () (error 'perform-primitive "Unimplemented primitive: ~a args: ~a"
                               primitive-number
                               (obj-slots args))))
   vm args))

;;===========================================================================

(define-primitive vm [1 b a] (boolean->obj vm (eq? a b)))
(define-primitive vm [2 x] (obj-class* vm x))
(define-primitive vm [4 o] (cond [(bv? o) (bytes-length (bv-bytes o))]
                                 [(obj? o) (slotCount o)]
                                 [(number? o) 0]
                                 [else (error 'execute "Primitive 4 failed")]))
(define-primitive vm [5 value target index]
  (slotAtPut target (- index 1) value)
  target)
(define-primitive vm [6 inner-ctx] ;; "new context execute"
  (execute vm inner-ctx))
(define-primitive vm [7 class count]
  (obj class (make-vector count (VM-nil vm))))

(define-primitive vm [10 b a] (+ a b)) ;; TODO: overflow
(define-primitive vm [11 n d] (quotient n d))
(define-primitive vm [12 n d] (modulo n d))
(define-primitive vm [14 b a] (boolean->obj vm (= a b)))
(define-primitive vm [15 b a] (* a b))
(define-primitive vm [16 a b] (- a b)) ;; NB. ordering

(define-primitive vm [18 v] (log-vm-info "DEBUG: value ~v class ~v" v (obj-class* vm v)))

(define-primitive vm [20 class count] (mkbv class (make-bytes count)))
(define-primitive vm [21 source index] (bytes-ref (bv-bytes source) (- index 1)))
(define-primitive vm [22 value target index]
  (bytes-set! (bv-bytes target) (- index 1) value)
  target)
(define-primitive vm [24 (unbv b) (unbv* av a)] (mkbv (obj-class av) (bytes-append a b)))
(define-primitive vm [26 (unbv a) (unbv b)] ;; NB. ordering
  (cond [(bytes<? a b) -1]
        [(bytes=? a b) 0]
        [(bytes>? a b) 1]))

(define-primitive vm [30 source index] (slotAt source (- index 1)))
(define-primitive vm [31 v o] (obj (obj-class o) (vector-append (obj-slots o) (vector v))))

(define-primitive vm [41 class (unstr filename)]
  (mkffiv class (open-output-file filename #:exists 'replace)))
(define-primitive vm [42 class (unstr filename)]
  (mkffiv class (open-input-file filename)))
(define-primitive vm [44 class (unffiv fh)]
  (match (read-bytes-line fh)
    [(? eof-object?) (VM-nil vm)]
    [bs (mkbv class bs)]))

;;---------------------------------------------------------------------------
;; GUI
;;---------------------------------------------------------------------------

(define-primitive vm [60 class] ;; make window
  (log-vm/gui-debug "Creating window")
  (mkffiv class (new smalltalk-frame% [label "Racket SmallWorld"])))
(define-primitive vm [61 (unffiv window) flag] ;; show/hide text window
  (log-vm/gui-debug "Show/hide window ~a" (eq? flag (VM-true vm)))
  (send window show (eq? flag (VM-true vm)))
  flag)
(define-primitive vm [62 (unffiv* wv window) (unffiv (list _item factory))] ;; set content pane
  (log-vm/gui-debug "Set content pane")
  (factory window)
  wv)
(define-primitive vm [63 (unffiv* wv window) height width] ;; set size
  (log-vm/gui-debug "Window resize ~ax~a" width height)
  (send window resize width height)
  wv)
(define-primitive vm [64 (unffiv* wv window) (unffiv (list _queue-item add-menu-bar-to))]
  ;; add menu to window
  (define mb (or (send window get-menu-bar)
                 (new menu-bar% [parent window])))
  (log-vm/gui-debug "Add menu to window")
  (add-menu-bar-to mb)
  wv)
(define-primitive vm [65 (unffiv* wv window) (unstr text)] ;; set title
  (log-vm/gui-debug "Set window title ~v" text)
  (send window set-label text)
  wv)
(define-primitive vm [66 window] ;; repaint window
  ;; nothing needed
  window)
(define-primitive vm [70 class (unstr label)] ;; new label panel
  (log-vm/gui-debug "Schedule label panel ~v" label)
  (define (create-label-in parent)
    (log-vm/gui-debug "Create label panel ~v" label)
    (new message% [parent parent] [label label]))
  (mkffiv class (list 'label create-label-in)))
(define-primitive vm [71 class (unstr label) action] ;; new button
  (define callback (block-callback vm action))
  (log-vm/gui-debug "Schedule button ~v" label)
  (define (create-button-in parent)
    (log-vm/gui-debug "Create button ~v" label)
    (new button%
         [label label]
         [parent parent]
         [callback (lambda args (queue-callback callback))]))
  (mkffiv class (list 'button create-button-in)))
(define-primitive vm [72 class] ;; new text line
  (log-vm/gui-debug "Schedule textfield")
  (define textfield-editor #f)
  (define (add-textfield-to parent)
    (set! textfield-editor (send (new text-field% [label #f] [parent parent]) get-editor))
    textfield-editor)
  (mkffiv class (list (lambda () textfield-editor) add-textfield-to)))
(define-primitive vm [73 class] ;; new text area
  (log-vm/gui-debug "Schedule textarea")
  (define editor (new text%))
  (define (add-editor-to frame)
    (log-vm/gui-debug "Create textarea")
    (new editor-canvas% [parent frame] [editor editor]))
  (mkffiv class (list (lambda () editor) add-editor-to)))
(define-primitive vm [74 class width height data] ;; new grid panel
  (log-vm/gui-debug "Schedule grid panel ~ax~a ~a" width height data)
  (define (create-grid-in parent)
    (log-vm/gui-debug "Create grid panel ~ax~a ~a" width height data)
    (define vp (new vertical-pane% [parent parent]))
    (for [(row height)]
      (define hp (new horizontal-pane% [parent vp]))
      (for [(col width)]
        (define i (+ col (* row width)))
        (when (< i (slotCount data))
          (match (slotAt data i)
            [(unffiv (list _ factory)) (factory hp)]))))
    vp)
  (mkffiv class (list 'grid create-grid-in)))
(define-primitive vm [75 class data action] ;; new list panel
  (define callback (block-callback vm action))
  (log-vm/gui-debug "Schedule listpanel ~a" data)
  (define lb #f)
  (define old-selection #f)
  (define (create-list-panel-in parent)
    (log-vm/gui-debug "Create listpanel ~a" data)
    (set! lb (new list-box%
                  [label #f]
                  [parent parent]
                  [choices (for/list [(c (obj-slots data))] (bv->string c))]
                  [callback (lambda _args
                              (log-vm/gui-debug "_args: ~v for listpanel ~a"
                                                _args
                                                (eq-hash-code lb))
                              (define selection (send lb get-selection))
                              (when (not (equal? old-selection selection))
                                (set! old-selection selection)
                                (queue-callback
                                 (lambda ()
                                   (log-vm/gui-debug "Item selected ~v" selection)
                                   (callback (if selection (+ selection 1) 0))))))]))
    (log-vm/gui-debug "The result is ~a" (eq-hash-code lb))
    lb)
  (mkffiv class (list (lambda () lb) create-list-panel-in)))
(define-primitive vm [76 class north south east west center] ;; new border panel
  (log-vm/gui-debug "Schedule borderpanel")
  (define (add-w w p)
    (when (not (eq? (VM-nil vm) w))
      (match w [(unffiv (list _ factory)) (factory p)])))
  (define (create-border-panel-in parent)
    (log-vm/gui-debug "Create borderpanel")
    (define vp (new vertical-pane% [parent parent]))
    (add-w north vp)
    (when (for/or [(w (list west center east))] (not (eq? (VM-nil vm) w)))
      (define hp (new horizontal-pane% [parent vp]))
      (add-w west hp)
      (add-w center hp)
      (add-w east hp))
    (add-w south vp)
    vp)
  (mkffiv class (list 'border-panel create-border-panel-in)))
(define-primitive vm [80 class (unffiv (list get-textarea _factory))] ;; content of text area
  (mkbv class (string->bytes/utf-8 (send (get-textarea) get-text))))
(define-primitive vm [81 class (unffiv (list get-textarea _factory))] ;; content of selected text area
  (define start (box 0))
  (define end (box 0))
  (send (get-textarea) get-position start end)
  (define has-selection (not (= (unbox start) (unbox end))))
  (mkbv class
        (string->bytes/utf-8 (send (get-textarea) get-text
                                   (if has-selection (unbox start) 0)
                                   (if has-selection (unbox end) 'eof)))))
(define-primitive vm [82 (unffiv (list get-textarea _factory)) (and textv (unstr text))] ;; set text area
  (log-vm/gui-debug "Update textarea ~v" text)
  (send (get-textarea) erase)
  (send (get-textarea) insert text)
  textv)
(define-primitive vm [83 (unffiv (list get-lb _factory))] ;; get selected index
  (log-vm/gui-debug "Get selected index")
  (define lb (get-lb))
  (define s (send lb get-selection))
  (if s (+ s 1) 0))
(define-primitive vm [84 (unffiv* lbv (list get-lb _factory)) data] ;; set list data
  (define lb (get-lb))
  (log-vm/gui-debug "Update list ~a data ~v" (eq-hash-code lb) data)
  (send lb set (for/list [(c (obj-slots data))] (bv->string c)))
  lbv)
(define-primitive vm [89 (unffiv (list get-textarea _factory)) (and textv (unstr text))] ;; set selected text area
  (define start (box 0))
  (define end (box 0))
  (send (get-textarea) get-position start end)
  (define has-selection (not (= (unbox start) (unbox end))))
  (if has-selection
      (send (get-textarea) insert text (unbox start) (unbox end))
      (begin (send (get-textarea) erase)
             (send (get-textarea) insert text)))
  textv)
(define-primitive vm [90 class (unstr title)] ;; new menu
  (define pending-items '())
  (define (queue-item i)
    (set! pending-items (cons i pending-items)))
  (define (add-menu-bar-to frame)
    (define m (new menu% [parent frame] [label title]))
    (for [(i (reverse pending-items))] (i m))
    m)
  (mkffiv class (list queue-item add-menu-bar-to)))
(define-primitive vm [91 (unffiv* menu (list queue-item _add-menu-bar-to)) (unstr title) action] ;; new menu item
  (define callback (block-callback vm action))
  (queue-item (lambda (m)
                (new menu-item%
                     [label title]
                     [parent m]
                     [callback (lambda args (queue-callback callback))])))
  menu)
(define-primitive vm [100 class] (mkffiv class (oneshot)))
(define-primitive vm [101 (unffiv o)] (oneshot-ref o))
(define-primitive vm [102 (unffiv o) v]
  (oneshot-set! o v)
  v)
(define-primitive vm [116]
  (let ((image-bytes (serialize-image vm)))
    (display-to-file image-bytes (VM-image-filename vm) #:exists 'replace)))
(define-primitive vm [117 _self] (exit))
(define-primitive vm [118 (unffiv* wv window) action] ;; "onWindow close b"
  (define callback (block-callback vm action))
  (send window set-close-handler (lambda (_frame) (queue-callback callback) (sleep 0.2)))
  wv)

;;---------------------------------------------------------------------------
;; END GUI
;;---------------------------------------------------------------------------

(define-primitive vm [119] (inexact->exact (round (current-inexact-milliseconds))))

;;===========================================================================

(define (doIt vm task)
  (define true-class (obj-class (VM-true vm))) ;; class True
  (define name (slotAt true-class 0)) ;; "a known string", namely the name of class True
  (define string-class (obj-class name)) ;; class String
  (define source (mkbv string-class (string->bytes/utf-8 task)))
  (define args (mkobj (VM-Array vm) source))
  (send-message vm (VM-nil vm) args (mkbv (VM-nil vm) #"doIt")))

(let* ((image-filename "SmallWorld/src/image")
       (vm (call-with-input-file image-filename (lambda (fh) (read-image image-filename fh)))))
  (log-vm-info "Sending 'SmallWorld startUp'...")
  (thread-wait (thread (lambda ()
                         (define result (doIt vm "SmallWorld startUp"))
                         (log-vm-info "Final startUp result: ~a" result)
                         (for [(a (current-command-line-arguments))]
                           (log-vm-info "Filing in ~a" a)
                           (doIt vm (format "(File openRead: '~a') fileIn" a)))
                         (yield))))
  (log-vm-info "... terminating."))

;;; Local Variables:
;;; eval: (put 'primitive-action 'scheme-indent-function 1)
;;; End:
