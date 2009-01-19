;; This file is included in a couple of different contexts to
;; initialise the primitive table and to build a bootstrap image, and
;; should only contain define-method forms.

(define-method (primListen (port *traits-number*)) primListenSocket
  (or (sdl-net-tcp-open (make-sdl-ip-address 0 0 0 0 port))
      *nil*))

(define-method (primConnect: (hostname *traits-string*) (port *traits-number*)) primConnectSocket
  (or (sdl-net-tcp-open (sdl-net-resolve-host hostname port))
      *nil*))

(define-method (accept (sock *traits-socket*)) primSocketAccept
  (or (accept-from-socket sock)
      *nil*))

(define-method (primPeerAddress (sock *traits-socket*)) primSocketPeerAddress
  (or (and-let* ((ipa (sdl-net-tcp-get-peer-address sock))
		 (hostname (sdl-net-resolve-ip ipa)))
	(vector hostname (sdl-ip-address-port ipa)))
      *nil*))

(define-method (close (sock *traits-socket*)) primSocketClose
  (sdl-net-tcp-close sock)
  *nil*)

(define-method (printOn: o (out *traits-socket*)) displayPrintString
  (let ((string-port (open-output-string)))
    (display (send/previous-method/missing-handler #f
						   (lambda (argv) "#<OBJECT>")
						   'printString
						   (vector o))
	     string-port)
    (let ((representation (get-output-string string-port)))
      (sdl-net-tcp-send-string out representation)
      *nil*)))

(define-method (printString (x *traits-root*)) rootPrintString
  (send as: x *traits-string*))

(define-method (primStringAppend: (s1 *traits-string*) (s2 *traits-string*)) primStringAppend
  (string-append s1 s2))

(define-method (- (n1 *traits-number*) (n2 *traits-number*)) numSub (- n1 n2))
(define-method (+ (n1 *traits-number*) (n2 *traits-number*)) numPlus (+ n1 n2))
(define-method (* (n1 *traits-number*) (n2 *traits-number*)) numTimes (* n1 n2))
(define-method (/ (n1 *traits-number*) (n2 *traits-number*)) numDiv (/ n1 n2))

(define-method (< (n1 *traits-number*) (n2 *traits-number*)) numLT (< n1 n2))
(define-method (> (n1 *traits-number*) (n2 *traits-number*)) numGT (> n1 n2))
(define-method (<= (n1 *traits-number*) (n2 *traits-number*)) numLE (<= n1 n2))
(define-method (>= (n1 *traits-number*) (n2 *traits-number*)) numGE (>= n1 n2))

(define-method (= (x *traits-root*) (y *traits-root*)) primEgal
  (let egal ((x x) (y y))
    (or (eq? x y)
	(cond
	 ((and (object? x) (object? y))
	  (let ((lx (object-layout x)) (sx (object-slots x))
		(ly (object-layout y)) (sy (object-slots y)))
	    (and (eq? lx ly)
		 (call-with-current-continuation
		  (lambda (escape)
		    (layout-for-each lx
				     (lambda (slot-name slot)
				       (case (slot-kind slot)
					 ((mutable) (escape #f))
					 ((immutable)
					  (let ((index (slot-index slot)))
					    (if (not (egal (vector-ref sx index)
							   (vector-ref sy index)))
						(escape #f))))
					 ((method) 'ignore-methods)
					 (else (error "Unknown slot kind in egal"
						      (slot-kind slot))))))
		    #t)))))
	 ((and (pair? x) (pair? y))
	  (and (egal (car x) (car y))
	       (egal (cdr x) (cdr y))))
	 ((and (vector? x) (vector? y))
	  (let ((len (vector-length x)))
	    (and (= len (vector-length y))
		 (let loop ((i 0))
		   (if (= i len)
		       #t
		       (and (egal (vector-ref x i)
				  (vector-ref y i))
			    (loop (+ i 1))))))))
	 ((and (number? x) (number? y))
	  (= x y))
	 (else #f)))))

(define-method (as: x (y *traits-string*)) rootAsString
  (if (eq? x *no-role*)
      "NoRole"
      (send name x)))

(define-method (as: (x '()) (y *traits-string*)) nilAsString "Nil")
(define-method (as: (x '#t) (y *traits-string*)) trueAsString "True")
(define-method (as: (x '#f) (y *traits-string*)) falseAsString "False")

(define-method (as: (x *traits-traits*) (y *traits-string*)) traitsAsString
  (string-append "#<"(send name x)" traits>"))

(define-method (as: (x *traits-string*) (y *traits-string*)) stringAsString
  (if (string? x)
      x
      (resend)))

(define-method (as: (x *traits-symbol*) (y *traits-string*)) symbolAsString
  (if (symbol? x)
      (symbol->string x)
      (resend)))

(define-method (as: (x *traits-number*) (y *traits-string*)) numberAsString
  (if (number? x)
      (number->string x)
      (resend)))

(define-method (new (c *traits-cell*)) newCell
  (clone-object *cell*))

(define-method (key (o *traits-pair*)) pairCar
  (car o))

(define-method (value (o *traits-pair*)) pairCdr
  (cdr o))

(define-method (size (v *traits-tuple*)) tupleSize
  (vector-length v))

(define-method (at: (v *traits-tuple*) (n *traits-number*)) tupleAt
  (vector-ref v n))

(define-method (-> (x *traits-root*) y) pairCons
  (cons x y))

(define-method (--> (c *traits-cell*) (m *traits-block*)) cellExtract
  (send applyWith: m (metalevel-extract-cell-value c)))

(define-method (peek (c *traits-cell*)) cellPeek
  (metalevel-peek-cell-value c))

(define-method (<-- (c *traits-cell*) v) cellInject
  (metalevel-inject-cell-value c v))

(define-method (withSlot:value: template (name *traits-symbol*) val) primAddSlot
  (let ((o (clone-object template)))
    (if (has-slot? o name)
	(set-slot! o name val)
	(add-slot! o name val #f 'immutable))
    o))

(define-method (fork: (loc *traits-location*) (block *traits-block*)) forkBlockInLocation
  (metalevel-spawn loc (lambda () (send do block)))
  *nil*)

(define-method (fileIn (filename *traits-string*)) stringFileIn
  (ThiNG-load-file filename))

(define-method (compileOneStatement (sock *traits-socket*)) primCompileOneStatement
  (let-values (((success ast) (parse-ThiNG (external-representation sock)
					   ThiNG-topexpr-parser
					   (make-char-provider-thunk-for-socket sock))))
    (if success
	(cons *true* (metalevel-eval `(block () (,ast))))
	(cons *false* ast))))

(define-method (saveImage (filename *traits-string*)) primSaveImage
  (debug 0 "Saving image...")
  (call-with-output-file filename
    (lambda (port)
      (write (serialize-image!) port)
      (newline port))))

(define-method (primQuit (r *traits-root*)) primQuit
  (shutdown-sdl!)
  *nil*)

;---------------------------------------------------------------------------

(define-method (handle (e *traits-sdl-event*)) handleBasicSdlEvent
  #t)

(define-method (handle (e (traits-for-sdl-event-type SDL_QUIT))) handleQuitSdlEvent
  (metalevel-stop!)
  #f)

(define-method (handle (e (traits-for-sdl-event-type SDL_MOUSEBUTTONDOWN))) handleSdlClick
  (let ((s2 (ttf-render-text-blended *system-font*
				     "(click)"
				     (make-sdl-color 255 255 255))))
    (sdl-blit-surface s2 #f
		      *video-surface* (make-sdl-rect (sdl-event-x e)
						     (sdl-event-y e)
						     0 0))
    (sdl-free-surface s2))
  (sdl-flip *video-surface*)
  (resend))

(define-method (handle (e (traits-for-sdl-event-type SDL_VIDEORESIZE))) handleSdlVideoResize
  (let ((w (sdl-event-w e))
	(h (sdl-event-h e)))
    (display (list 'resize w h))
    (newline)
    (sdl-set-video-mode w h 0 (+ SDL_HWSURFACE
				 SDL_HWPALETTE
				 SDL_RESIZABLE
				 SDL_DOUBLEBUF)))
  (sdl-fill-rect *video-surface*
		 (make-sdl-rect 0 0
				(sdl-surface-width *video-surface*)
				(sdl-surface-height *video-surface*))
		 (sdl-map-rgb (sdl-surface-pixel-format *video-surface*) 0 0 64))
  (let ((s2 (ttf-render-text-blended *system-font*
				     "Hello, world!"
				     (make-sdl-color 255 255 255))))
    (sdl-blit-surface s2 #f *video-surface* (make-sdl-rect 0 0 0 0))
    (sdl-free-surface s2))
  (sdl-flip *video-surface*)
  (resend))

(define-method (handle (e (traits-for-sdl-event-type SDL_KEYDOWN))) handleSdlKeydown
  (let* ((i (sdl-event-sym e))
	 (c (integer->char i)))
    (if (or (= i 27) (memv c '(#\q #\Q)))
	(shutdown-sdl!)
	(begin (display (list 'got-key c))
	       (newline))))
  (resend))
