#lang racket/gui
;; Loader for images (version 1 format) from Russell Allen's 2015
;; variant of SmallWorld, a Tim Budd-authored Little Smalltalk
;; descendant.

(require racket/struct)
(require racket/async-channel)

(define-logger vm)
(define-logger vm/gui)

(struct obj ([class #:mutable] slots)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer (lambda (o) (format "obj:~a" (obj-class-name o)))
                                     (lambda (o) (if (equal? #"Array" (obj-class-name o))
                                                     (list (vector->list (obj-slots o)))
                                                     '()))))])
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

(define (obj-class-name o)
  (define c (obj-class o))
  (if (and (positive? (slotCount c))
           (bv? (slotAt c 0)))
      (bv-bytes (slotAt c 0))
      #"???"))

(struct VM (nil true false Array Block Context Integer))

(define (read-image fh)

  (define (maybe-next-int #:signed? [signed? #f])
    (define bs (read-bytes 4 fh))
    (if (eof-object? bs)
        bs
        (integer-bytes->integer bs signed? #t)))

  (define (next-int #:signed? [signed? #f])
    (define i (maybe-next-int #:signed? signed?))
    (when (eof-object? i) (error 'read-image "Early EOF"))
    i)

  (let ((image-version (next-int))
        (expected-version 1))
    (when (not (= image-version expected-version))
      (error 'read-image "Wrong image version: got ~a, expected ~a"
             image-version
             expected-version)))

  (define object-table
    (let loop ((acc '()))
      (define (emit x) (loop (cons x acc)))
      (match (maybe-next-int)
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
      (vector-ref object-table 6)))

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

(define (clone-array a [start 0] [count (- (slotCount a) start)])
  (define b (obj (obj-class a) (make-vector count)))
  (for [(i (in-range count))]
    (slotAtPut b i (slotAt a (+ i start))))
  b)

(define (boolean->obj vm b)
  (if b (VM-true vm) (VM-false vm)))

(define (lookup-method vm class selector)
  (define name-bytes (bv-bytes selector))
  (let search ((class class))
    (and (not (eq? class (VM-nil vm)))
         (or (search-class-method-dictionary class name-bytes)
             (search (slotAt class 1))))))

(define (store-registers! ctx ip stack-top)
  (slotAtPut ctx 4 ip)
  (slotAtPut ctx 5 stack-top))

(define (send-message* vm ctx ip stack-top arguments class selector)
  (store-registers! ctx ip stack-top)
  (match (lookup-method vm class selector)
    [#f
     (match (lookup-method vm class (mkbv (obj-class selector) #"doesNotUnderstand:"))
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

(define (obj-class* vm o)
  (if (number? o)
      (VM-Integer vm)
      (obj-class o)))

(define (send-message vm ctx ip stack-top arguments selector)
  (log-vm-debug "sending: ~a ~a" selector arguments)
  (send-message* vm ctx ip stack-top arguments (obj-class* vm (slotAt arguments 0)) selector))

(define (block-callback vm block)
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
      (thread (lambda () (execute vm ctx))))))

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

  (define-syntax-rule (primitive-action [arg-pat ...] body ...)
    (match-let* ([arg-pat (pop!)] ...) (push-and-continue (let () body ...))))

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
          (send-message* vm ctx ip stack-top new-arguments super selector)])]

      [13 ;; Primitive; low = arg count; next byte = primitive number
       (define primitive-number (next-byte!))
       (log-vm-debug "primitive ~a (arg count = ~a)" primitive-number low)
       (match primitive-number
         [1 (primitive-action [a b] (boolean->obj vm (eq? a b)))]
         [2 (primitive-action [x] (obj-class* vm x))]
         [4 (primitive-action [o] (cond [(bv? o) (bytes-length (bv-bytes o))]
                                        [(obj? o) (slotCount o)]
                                        [(number? o) 0]
                                        [else (error 'execute "Primitive 4 failed")]))]
         [5 (primitive-action [index target value]
              (slotAtPut target (- index 1) value)
              target)]
         [6 (primitive-action [inner-ctx] ;; "new context execute"
              (execute vm inner-ctx))]
         [7 (primitive-action [count class]
              (obj class (make-vector count (VM-nil vm))))]
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
         [11 (primitive-action [d n] (quotient n d))]
         [12 (primitive-action [d n] (modulo n d))]
         [14 (primitive-action [a b] (boolean->obj vm (= a b)))]
         [15 (primitive-action [a b] (* a b))]
         [16 (primitive-action [b a] (- a b))] ;; NB. ordering
         [20 (primitive-action [count class] (mkbv class (make-bytes count)))]
         [21 (primitive-action [index source] (bytes-ref (bv-bytes source) (- index 1)))]
         [22 (primitive-action [index target value]
               (bytes-set! (bv-bytes target) (- index 1) value)
               target)]
         [24 (primitive-action [(unbv* av a) (unbv b)] (mkbv (obj-class av) (bytes-append a b)))]
         [26 (primitive-action [(unbv b) (unbv a)] ;; NB. ordering
               (cond [(bytes<? a b) -1]
                     [(bytes=? a b) 0]
                     [(bytes>? a b) 1]))]
         [30 (primitive-action [index source] (slotAt source (- index 1)))]
         [31 (primitive-action [o v] (obj (obj-class o) (vector-append (obj-slots o) (vector v))))]
         [34 (VM-nil vm)] ;; "thread kill"
         [35 (push-and-continue ctx)]
         [36 (push-and-continue (pop-multiple! low))] ;; "fast array creation"

         ;;---------------------------------------------------------------------------
         ;; GUI
         ;;---------------------------------------------------------------------------

         [60 ;; make window
          (primitive-action [class]
            (log-vm/gui-debug "Creating window")
            (mkffiv class (new smalltalk-frame% [label "Racket SmallWorld"])))]
         [61 ;; show/hide text window
          (primitive-action [flag (unffiv window)]
            (log-vm/gui-debug "Show/hide window ~a" (eq? flag (VM-true vm)))
            (send window show (eq? flag (VM-true vm)))
            flag)]
         [62 ;; set content pane
          (primitive-action [(unffiv (list _item factory)) (unffiv* wv window)]
            (log-vm/gui-debug "Set content pane")
            (factory window)
            wv)]
         [63 ;; set size
          (primitive-action [width height (unffiv* wv window)]
            (log-vm/gui-debug "Window resize ~ax~a" width height)
            (send window resize width height)
            wv)]
         [64 ;; add menu to window
          (primitive-action [(unffiv (list _queue-item add-menu-bar-to))
                             (unffiv* wv window)]
            (define mb (or (send window get-menu-bar)
                           (new menu-bar% [parent window])))
            (log-vm/gui-debug "Add menu to window")
            (add-menu-bar-to mb)
            wv)]
         [65 ;; set title
          (primitive-action [(unstr text) (unffiv* wv window)]
            (log-vm/gui-debug "Set window title ~v" text)
            (send window set-label text)
            wv)]
         [66 ;; repaint window
          (primitive-action [window]
            ;; nothing needed
            window)]
         [70 ;; new label panel
          (primitive-action [(unstr label) class]
            (log-vm/gui-debug "Schedule label panel ~v" label)
            (define (create-label-in parent)
              (log-vm/gui-debug "Create label panel ~v" label)
              (new message% [parent parent] [label label]))
            (mkffiv class (list 'label create-label-in)))]
         [71 ;; new button
          (primitive-action [action (unstr label) class]
            (define callback (block-callback vm action))
            (log-vm/gui-debug "Schedule button ~v" label)
            (define (create-button-in parent)
              (log-vm/gui-debug "Create button ~v" label)
              (new button%
                   [label label]
                   [parent parent]
                   [callback (lambda args (queue-callback callback))]))
            (mkffiv class (list 'button create-button-in)))]
         [74 ;; new grid panel
          (primitive-action [data height width class]
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
            (mkffiv class (list 'grid create-grid-in)))]
         [73 ;; new text area
          (primitive-action [class]
            (log-vm/gui-debug "Schedule textarea")
            (define editor (new text%))
            (define (add-editor-to frame)
              (log-vm/gui-debug "Create textarea")
              (new editor-canvas% [parent frame] [editor editor]))
            (mkffiv class (list editor add-editor-to)))]
         [75 ;; new list panel
          (primitive-action [action data class]
            (define callback (block-callback vm action))
            (log-vm/gui-debug "Schedule listpanel ~a" data)
            (define lb #f)
            (define old-selection #f)
            (define (create-list-panel-in parent)
              (log-vm/gui-debug "Create listpanel ~a" data)
              (set! lb (new list-box%
                            [label #f]
                            [parent parent]
                            [choices (for/list [(c (obj-slots data))] (match-define (unstr t) c) t)]
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
            (mkffiv class (list (lambda () lb) create-list-panel-in)))]
         [76 ;; new border panel
          (primitive-action [center west east south north class]
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
            (mkffiv class (list 'border-panel create-border-panel-in)))]
         [80 ;; content of text area
          (primitive-action [(unffiv (list textarea _factory)) class]
            (mkbv class (string->bytes/utf-8 (send textarea get-text))))]
         [81 ;; content of selected text area
          (primitive-action [(unffiv (list textarea _factory)) class]
            (define start (box 0))
            (define end (box 0))
            (send textarea get-position start end)
            (define has-selection (not (= (unbox start) (unbox end))))
            (mkbv class
                  (string->bytes/utf-8 (send textarea get-text
                                             (if has-selection (unbox start) 0)
                                             (if has-selection (unbox end) 'eof)))))]
         [82 ;; set text area
          (primitive-action [(and textv (unstr text)) (unffiv (list textarea _factory))]
            (log-vm/gui-debug "Update textarea ~v" text)
            (send textarea erase)
            (send textarea insert text)
            textv)]
         [83 ;; get selected index
          (primitive-action [(unffiv (list get-lb _factory))]
            (log-vm/gui-debug "Get selected index")
            (define lb (get-lb))
            (define s (send lb get-selection))
            (if s (+ s 1) 0))]
         [84 ;; set list data
          (primitive-action [data (unffiv* lbv (list get-lb _factory))]
            (define lb (get-lb))
            (log-vm/gui-debug "Update list ~a data ~v" (eq-hash-code lb) data)
            (send lb clear)
            (for [(c (obj-slots data))] (match-define (unstr t) c) (send lb append t))
            lbv)]
         [89 ;; set selected text area
          (primitive-action [(and textv (unstr text)) (unffiv (list textarea _factory))]
            (define start (box 0))
            (define end (box 0))
            (send textarea get-position start end)
            (define has-selection (not (= (unbox start) (unbox end))))
            (if has-selection
                (send textarea insert text (unbox start) (unbox end))
                (begin (send textarea erase)
                       (send textarea insert text)))
            textv)]
         [90 ;; new menu
          (primitive-action [(unstr title) class]
            (define pending-items '())
            (define (queue-item i)
              (set! pending-items (cons i pending-items)))
            (define (add-menu-bar-to frame)
              (define m (new menu% [parent frame] [label title]))
              (for [(i (reverse pending-items))] (i m))
              m)
            (mkffiv class (list queue-item add-menu-bar-to)))]
         [91 ;; new menu item
          (primitive-action [action (unstr title) (unffiv* menu (list queue-item _add-menu-bar-to))]
            (define callback (block-callback vm action))
            (queue-item (lambda (m)
                          (new menu-item%
                               [label title]
                               [parent m]
                               [callback (lambda args (queue-callback callback))])))
            menu)]
         [100 (primitive-action [class]
                (define ch (make-async-channel))
                (mkffiv class ch))]
         [101 (primitive-action [(unffiv ch)]
                (define v (async-channel-get ch))
                (async-channel-put ch v)
                v)]
         [102 (primitive-action [v (unffiv ch)]
                (async-channel-try-get ch)
                (async-channel-put ch v)
                v)]
         [117 (exit)]
         [118 ;; "onWindow close b"
          (primitive-action [action (unffiv* wv window)]
            (define callback (block-callback vm action))
            (send window set-close-handler (lambda (_frame) (queue-callback callback) (sleep 0.2)))
            wv)]

         ;;---------------------------------------------------------------------------
         ;; END GUI
         ;;---------------------------------------------------------------------------

         [119 (push-and-continue (inexact->exact (round (current-inexact-milliseconds))))]

         [_ (error 'execute "Unimplemented primitive: ~a stack: ~a"
                   primitive-number
                   (obj-slots stack))])]))

  (interpret))

(define (doIt vm task)
  (define true-class (obj-class (VM-true vm))) ;; class True
  (define name (slotAt true-class 0)) ;; "a known string", namely the name of class True
  (define string-class (obj-class name)) ;; class String
  (define doIt-method (search-class-method-dictionary string-class #"doIt"))
  (when (not doIt-method)
    (error 'doIt "Can't find doIt method via class True etc"))
  (define source (mkbv string-class (string->bytes/utf-8 task)))
  (define args (mkobj (VM-Array vm) source))
  (define ctx (build-context vm (VM-nil vm) args doIt-method))
  (execute vm ctx))

(let ((vm (call-with-input-file "SmallWorld/src/image" read-image)))
  (printf "Sending 'SmallWorld startUp'...\n")
  (thread-wait (thread (lambda ()
                         (define result (doIt vm "SmallWorld startUp"))
                         (log-vm-info "Final startUp result: ~a" result)
                         (yield))))
  (printf "... terminating.\n"))

;;; Local Variables:
;;; eval: (put 'primitive-action 'scheme-indent-function 1)
;;; End:
