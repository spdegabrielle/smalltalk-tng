;; ---------------------------------------------------------------------------
;; tng.scm - ThiNG main file. Load this with sdl-csi.
;; ---------------------------------------------------------------------------

;; The following is required because chicken by default has
;; trailing-':' keyword mode, which makes literal symbols that end
;; with ':' behave wrongly. If you don't set the keyword-style to
;; #:none, then you get this:
;;
;; (eq? 'a: (string->symbol "a:")) ==> #f
;;
(require 'extras)

(load "macros.scm")
(require 'util)
(require 'oo)
(require 'kernel)
(require 'parsetng)
(require 'compile)
(require 'interp)
(require 'ui)
(require 'image)

(reset-primitive-table!)

(define boot-image-name "BOOTSTRAP.image")

(if (file-exists? boot-image-name)
    (call-with-input-file boot-image-name
      (lambda (port)
	(let ((image (read port)))
	  (deserialize-image! image))))
    (bootstrap-image!))

;; Be careful not to invoke any methods before the image is prepared!
;; If you turn on debug, and the image-loader or -bootstrapper prints
;; any objects, and the following record-printer is installed, then
;; things break.
;;
(define-record-printer (object o out)
  (display (send/previous-method/missing-handler #f
						 (lambda (argv) "#<OBJECT>")
						 'printString
						 (vector o))
	   out))

(metalevel-spawn *nil* (lambda () (metalevel-eval `(send "do" ((ref "BootBlock"))))))
(ui-mainloop)
(exit 0)
