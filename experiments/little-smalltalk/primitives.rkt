#lang racket/gui

(provide *primitive-handlers*
         *primitive-code-snippets*
         define-primitive
         gen:vm-callback

         ;; These are referred to by spliced-in S-expression code
         ;; fragments, indirectly via the eval namespace used to
         ;; instantiate compiled code.
         ;;
         vm-block-callback
         smalltalk-frame%
         oneshot
         oneshot-set!
         oneshot-ref
         log-vm/gui-debug
         log-vm/gui-info
         log-vm/gui-warning
         log-vm/gui-error)

(require racket/generic)
(require "object-memory.rkt")

(define-logger vm)
(define-logger vm/gui)

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

(define-generics vm-callback
  (vm-block-callback vm-callback action))

;;===========================================================================

(define (oneshot)
  (thread (lambda ()
            (define (no-value waiters)
              (match (thread-receive)
                [(list 'get ch) (no-value (cons ch waiters))]
                [(list 'set v)
                 (for [(ch waiters)] (channel-put ch v))
                 (value v)]))
            (define (value v)
              (match (thread-receive)
                [(list 'get ch)
                 (channel-put ch v)
                 (value v)]
                [(list 'set v)
                 (value v)]))
            (no-value '()))))

(define (oneshot-set! o v)
  (thread-send o (list 'set v)))

(define (oneshot-ref o)
  (define ch (make-channel))
  (thread-send o (list 'get ch))
  (channel-get ch))

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
;; 6 - "new context execute"
(define-primitive vm [7 class count]
  (obj class (make-vector count (VM-nil vm))))

(define-primitive vm [10 a b] (+ a b)) ;; TODO: overflow
(define-primitive vm [11 n d] (quotient n d))
(define-primitive vm [12 n d] (modulo n d))
(define-primitive vm [13 a b] (boolean->obj vm (< a b)))
(define-primitive vm [14 a b] (boolean->obj vm (= a b)))
(define-primitive vm [15 a b] (* a b))
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

(define-primitive vm [29 filename] (save-image-to-file vm (bv->string filename)))

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

(define smalltalk-frame%
  (class frame%
    (field [close-handler void])
    (define/public (set-close-handler new-handler)
      (set! close-handler new-handler))
    (define/augment (on-close)
      (close-handler this))
    (super-new)))

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
  (define callback (vm-block-callback vm action))
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
  (define callback (vm-block-callback vm action))
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
  (define callback (vm-block-callback vm action))
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
;; 116 - save image to preset filename
(define-primitive vm [117 _self] (exit))
(define-primitive vm [118 (unffiv* wv window) action] ;; "onWindow close b"
  (define callback (vm-block-callback vm action))
  (send window set-close-handler (lambda (_frame) (queue-callback callback) (sleep 0.2)))
  wv)

;;---------------------------------------------------------------------------
;; END GUI
;;---------------------------------------------------------------------------

(define-primitive vm [119] (inexact->exact (round (current-inexact-milliseconds))))
