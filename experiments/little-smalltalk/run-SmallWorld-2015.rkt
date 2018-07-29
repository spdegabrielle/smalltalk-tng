#lang racket/gui
;; Loader for images (version 1 format) from Russell Allen's 2015
;; variant of SmallWorld, a Tim Budd-authored Little Smalltalk
;; descendant.

(require racket/struct)
(require racket/bytes)
(require "object-memory.rkt")
(require "primitives.rkt")

(define-logger vm)

(struct int-VM VM (cache image-filename)
  #:methods gen:vm-callback
  [(define (vm-block-callback vm block)
     ;; Runs block in a new thread
     (lambda args
       (let ((ctx (clone-array block)))
         (define argument-location (slotAt ctx 7))
         (for [(i (in-naturals argument-location)) (arg (in-list args))]
           (slotAtPut (slotAt ctx 2) i arg))
         (slotAtPut ctx 3 (mkarray vm (slotCount (slotAt ctx 3))))
         (slotAtPut ctx 4 (slotAt ctx 9)) ;; reset IP to correct block offset
         (slotAtPut ctx 5 0) ;; zero stack-top
         (slotAtPut ctx 6 (VM-nil vm)) ;; no previous context
         (thread (lambda () (execute vm ctx))))))])

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

(define (clone-array a [start 0] [count (- (slotCount a) start)])
  (define b (obj (obj-class a) (make-vector count)))
  (for [(i (in-range count))]
    (slotAtPut b i (slotAt a (+ i start))))
  b)

(define (lookup-method/cache vm class selector)
  (define name-bytes (bv-bytes selector))
  (define class-cache (hash-ref! (int-VM-cache vm) class make-weak-hash))
  (hash-ref! class-cache
             name-bytes
             (lambda ()
               (lookup-method vm class (bv-bytes selector)))))

(define (store-registers! ctx ip stack-top)
  (slotAtPut ctx 4 ip)
  (slotAtPut ctx 5 stack-top))

(define (send-message* vm ctx ip stack-top arguments class selector)
  (store-registers! ctx ip stack-top)
  (match (lookup-method/cache vm class selector)
    [#f
     (match (lookup-method/cache vm class (mkbv (obj-class selector) #"doesNotUnderstand:"))
       [#f
        (error 'send-message* "Unhandled selector ~a at class ~a" selector class)]
       [dnu-method
        (log-vm-warning "DNU -- arguments ~a class ~a selector ~a" arguments class selector)
        (execute vm (build-context vm
                                   ctx
                                   (mkobj (VM-Array vm)
                                          (slotAt arguments 0)
                                          (mkobj (VM-Array vm)
                                                 selector
                                                 (clone-array arguments)))
                                   dnu-method))])]
    [new-method
     (execute vm (build-context vm ctx arguments new-method))]))

(define (send-message vm ctx ip stack-top arguments selector)
  (log-vm-debug "sending: ~a ~a" selector arguments)
  (send-message* vm ctx ip stack-top arguments (obj-class* vm (slotAt arguments 0)) selector))

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
       (send-message vm ctx ip stack-top new-arguments (slotAt literals low))]

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
             (send-message vm ctx ip stack-top new-arguments selector)))]

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

         [_ (define args (pop-multiple! low))
            (define handler (hash-ref *primitive-handlers* primitive-number))
            (push-and-continue (handler vm args))])]

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
          (send-message* vm ctx ip stack-top new-arguments super selector)])]))

  (interpret))

;;===========================================================================

(define-primitive vm [6 inner-ctx] ;; "new context execute"
  (execute vm inner-ctx))

(define-primitive vm [116] (save-image-to-file vm (int-VM-image-filename vm)))

;;===========================================================================

(let* ((image-filename "SmallWorld/src/image")
       (vm (call-with-input-file image-filename
             (lambda (fh)
               (read-image fh int-VM (list (make-weak-hasheq) image-filename))))))
  (boot-image vm
              (lambda (vm source)
                (define args (mkobj (VM-Array vm) source))
                (define doIt-method (search-class-method-dictionary (obj-class source) #"doIt"))
                (when (not doIt-method) (error 'doIt "Can't find doIt method via class True etc"))
                (execute vm (build-context vm (VM-nil vm) args doIt-method)))
              (current-command-line-arguments)))
