(require 'srfi-1)
(require 'sdl)

(if (zero? (sdl-was-init SDL_INIT_VIDEO))
    (error "Please initialise SDL (use sdl-csi)."))

(ttf-init)
(sdl-net-init)

(define *system-font*
  (or (ttf-open-font "/sw/lib/X11/fonts/applettf/Monaco.ttf" 11)
      (ttf-open-font "/usr/share/fonts/truetype/ttf-bitstream-vera/VeraMono.ttf" 11)))

(define *event-type-map* (make-hash-table eq?))

(define (traits-for-sdl-event-type t)
  (or (hash-table-ref *event-type-map* t #f)
      (error "No traits for event type" t)))

(let-syntax ((def-sdl-event-type
	       (syntax-rules ()
		 ((_ (global-var sdl-event-type) ...)
		  (begin
		    (define global-var '*) ...)))))
  (include "sdl-events.scm"))

(define (update-sdl-event-type-map!)
  (let-syntax ((def-sdl-event-type
		 (syntax-rules ()
		   ((_ (global-var sdl-event-type) ...)
		    (begin
		      (hash-table-set! *event-type-map* sdl-event-type global-var) ...)))))
    (include "sdl-events.scm")))

(push! global-load-hooks
       (lambda ()
	 (let-syntax ((def-sdl-event-type
			(syntax-rules ()
			  ((_ (global-var sdl-event-type) ...)
			   (begin
			     (set! global-var (hash-table-ref *image-root* 'global-var)) ...
			     (update-sdl-event-type-map!))))))
	   (include "sdl-events.scm"))))

(push! global-store-hooks
       (lambda ()
	 (let-syntax ((def-sdl-event-type
			(syntax-rules ()
			  ((_ (global-var sdl-event-type) ...)
			   (begin
			     (hash-table-set! *image-root* 'global-var global-var) ...)))))
	   (include "sdl-events.scm"))))

(push! bootstrap-hooks
       (lambda ()
	 (let-syntax ((def-sdl-event-type
			(syntax-rules ()
			  ((_ (global-var sdl-event-type) ...)
			   (begin
			     (set! global-var (make-traits (symbol->string 'sdl-event-type)
							   `(#(sdlEvent ,*traits-sdl-event*)
							     (sdlEventNumber ,sdl-event-type))))
			     ...
			     (update-sdl-event-type-map!))))))
	   (include "sdl-events.scm"))))

(let ((old-hook (primitive-traits-hook)))
  (primitive-traits-hook
   (lambda (o)
     (cond
      ((sdl-tcp-socket? o) *traits-socket*)
      ((sdl-surface? o) *traits-sdl-surface*)
      ((sdl-event? o) (traits-for-sdl-event-type (sdl-event-type o)))
      ((ttf-font? o) *traits-ttf-font*)
      (else (old-hook o))))))

(sdl-wm-set-caption "ThiNG" "ThiNG")

(define (shutdown-sdl!)
  (let ((e (make-sdl-event)))
    (sdl-event-type-set! e SDL_QUIT)
    (sdl-push-event e)))

(define *socket-set* (sdl-net-alloc-socket-set 100))
(define *active-sockets* '())
(define *the-eof-object* (read-char (open-input-string "")))

(define (activate-socket! sock suspension)
  (push! *active-sockets* (cons sock suspension))
  (debug 1 "Adding "sock" to set "*socket-set*)
  (sdl-net-tcp-add-socket! *socket-set* sock))

(define (wait-for-socket-activity! sock)
  (metalevel-suspend-thread
   (lambda (suspension)
     (activate-socket! sock suspension))))

(define (read-from-socket sock)
  (wait-for-socket-activity! sock)
  (sdl-net-tcp-recv-string sock 4096))

(define (accept-from-socket sock)
  (wait-for-socket-activity! sock)
  (sdl-net-tcp-accept sock))

(define (make-char-provider-thunk-for-socket sock)
  (let ((state "")
	(len 0)
	(index 0))
    (define (provider)
      (cond
       ((eof-object? state) state)
       ((>= index len)
	(let ((new-state (read-from-socket sock)))
	  (if (string? new-state)
	      (begin
		(set! state new-state)
		(set! len (string-length state))
		(set! index 0)
		(provider))
	      (begin
		(set! state *the-eof-object*)
		(set! len 0)
		(set! index 0)
		(provider)))))
       (else
	(let ((result (string-ref state index)))
	  (set! index (+ index 1))
	  result))))
    provider))

(define (check-socket-set/delay delay-ms)
  (let ((next-event-time (+ (get-time-of-day) (/ delay-ms 1000.0)))
	(result (sdl-net-check-sockets *socket-set* 0)))
    (if (and result (positive? result))
	(let-values (((ready unready) (partition (lambda (record)
						   (sdl-net-socket-ready? (car record)))
						 *active-sockets*)))
	  (set! *active-sockets* unready)
	  (for-each (lambda (record)
		      (let ((sock (car record))
			    (suspension (cdr record)))
			(debug 1 "Removing "sock" from set "*socket-set*)
			(sdl-net-tcp-del-socket! *socket-set* sock)
			(metalevel-resume-thread! suspension sock)))
		    ready)))
    (metalevel-run-runnable-suspensions next-event-time)))

(define *video-surface* #f)

(define (discover-best-resolution!)
  (let loop ((resolutions '(
			    ;;(1600 1200) (1280 1024) (1024 768) (800 600)
			    (640 480))))
    (if (null? resolutions)
	(error "No resolution supported.")
	(let* ((res (car resolutions))
	       (maxx (car res))
	       (maxy (cadr res))
	       (s (sdl-set-video-mode maxx maxy 0 (+ SDL_HWSURFACE
						     ;;SDL_FULLSCREEN
						     SDL_HWPALETTE
						     SDL_RESIZABLE
						     SDL_DOUBLEBUF))))
	  (if (not (sdl-surface-pointer s))
	      (loop (cdr resolutions))
	      (set! *video-surface* s))))))

(define (ui-mainloop)
  (discover-best-resolution!)
  (sdl-fill-rect *video-surface*
		 (make-sdl-rect 0 0
				(sdl-surface-width *video-surface*)
				(sdl-surface-height *video-surface*))
		 (sdl-map-rgb (sdl-surface-pixel-format *video-surface*) 0 0 255))
  (sdl-flip *video-surface*)

  (let ((start-time (get-time-of-day)))
    (let loop ((count 1))
      (sdl-add-absolute-timer! (+ start-time (* count *invocation-count-update-interval*))
			       (lambda ()
				 (decay-invocation-counts!)
				 (loop (+ count 1))))))

  (do ()
      ((metalevel-stopped?))
    (let ((event (make-sdl-event)))
      (sdl-wait-event!* check-socket-set/delay event)
      (metalevel-spawn *nil* (lambda () (send handle event)))))

  (sdl-net-quit)
  (ttf-quit)
  (sdl-quit))
