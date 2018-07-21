#lang racket

(provide (struct-out obj)
         (struct-out bv)
         (struct-out ffiv)
         (struct-out VM)

         obj-class*

         mkobj
         mkbv
         mkffiv

         boolean->obj

         slotCount
         slotAt
         slotAtPut

         unbv
         unbv*
         unstr
         unffiv
         unffiv*

         bv->string
         obj-class-name
         search-class-method-dictionary
         lookup-method

         read-image
         serialize-image

         boot-image)

(define-logger vm)

(require racket/struct)

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

(struct VM (nil true false Array Block Context Integer))

(define (obj-class* vm o)
  (if (number? o)
      (VM-Integer vm)
      (obj-class o)))

(define (mkobj cls . fields) (obj cls (list->vector fields)))
(define (mkbv cls bs . fields) (bv cls (list->vector fields) bs))
(define (mkffiv cls value) (ffiv cls '#() value))

(define (boolean->obj vm b)
  (if b (VM-true vm) (VM-false vm)))

(define (slotCount o) (vector-length (obj-slots o)))
(define (slotAt o i) (vector-ref (obj-slots o) i))
(define (slotAtPut o i v) (vector-set! (obj-slots o) i v))

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

(define (search-class-method-dictionary c name-bytes)
  (define methods (slotAt c 2))
  (for/first [(m (obj-slots methods))
              #:when (equal? name-bytes (bv-bytes (slotAt m 0)))]
    m))

(define (lookup-method vm class name-bytes)
  (let search ((class class))
    (and (not (eq? class (VM-nil vm)))
         (or (search-class-method-dictionary class name-bytes)
             (search (slotAt class 1))))))

(define (read-image fh make-vm extra-make-vm-args)

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

  (apply make-vm
         (vector-ref object-table 0)
         (vector-ref object-table 1)
         (vector-ref object-table 2)
         (vector-ref object-table 3)
         (vector-ref object-table 4)
         (vector-ref object-table 5)
         (vector-ref object-table 6)
         extra-make-vm-args))

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
           (push-int! (+ (vector-length slots) 4))
           (push-int! 2)
           (push-int! (object->index class))
           (push-int! (vector-length slots))
           (for [(s slots)] (push-int! (object->index s)))]))
      (loop)))

  (bytes-append* (reverse output-rev)))

(define (boot-image vm evaluator files-to-file-in)
  (define (doIt task)
    (define true-class (obj-class (VM-true vm))) ;; class True
    (define name (slotAt true-class 0)) ;; "a known string", namely the name of class True
    (define string-class (obj-class name)) ;; class String
    (define source (mkbv string-class (string->bytes/utf-8 task)))
    (evaluator vm source))

  (log-vm-info "Sending 'SmallWorld startUp'...")
  (thread-wait (thread (lambda ()
                         (define result (doIt "SmallWorld startUp"))
                         (log-vm-info "Final startUp result: ~a" result)
                         (for [(a files-to-file-in)]
                           (log-vm-info "Filing in ~a" a)
                           (doIt (format "(File openRead: '~a') fileIn" a)))
                         ;; (yield)
                         )))
  (log-vm-info "... boot-image complete."))
