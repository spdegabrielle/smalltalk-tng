; GUI.SS  Graphical User Interface for LISP2
;
; (C) 1995 Tony Garnock-Jones
; tonyg@kcbbs.gen.nz
;

;----------------------------------------------------------------------------;
; Graphics toolbox calls.
;----------------------------------------------------------------------------;

(require 'form)

(define (rectangle rule thickness x y w h)
    (apply BitBlt screen-form '() '() rule
                  x y thickness h
                  (append clipping-rectangle '(0 0)))
    (apply BitBlt screen-form '() '() rule
                  x y w thickness
                  (append clipping-rectangle '(0 0)))
    (apply BitBlt screen-form '() '() rule
                  (+ x (- w thickness)) y thickness h
                  (append clipping-rectangle '(0 0)))
    (apply BitBlt screen-form '() '() rule
                  x (+ y (- h thickness)) w thickness
                  (append clipping-rectangle '(0 0))))

(define (fill-rect shade rule x y w h)
    (apply BitBlt screen-form '() shade rule
                  x y w h
                  (append clipping-rectangle '(0 0))))

;----------------------------------------------------------------------------;
; The window system proper.
;----------------------------------------------------------------------------;

;;; Class <rectangle>
;;;
;;; Represents a rectangle; used in screen geometry calculations.

(define-class <rectangle> <object> (x y w h))

;;; (<rectangle> new: x y w h)
;;;
;;; Creates and returns a rectangle with the specified coordinates.

(define-class-method <rectangle> (self new: x y w h)
    (let ((n (self new)))
        (n set: <rectangle> x x)
        (n set: <rectangle> y y)
        (n set: <rectangle> w w)
        (n set: <rectangle> h h)
        n))

;;; (rect x2)
;;; (rect y2)
;;;
;;; Return the coordinates of the right-hand lower corner of the rectangle.

(define-method <rectangle> (self x2)
    (+ (self : <rectangle> x) (self : <rectangle> w)))

(define-method <rectangle> (self y2)
    (+ (self : <rectangle> y) (self : <rectangle> h)))

;;; (rect write-to: port)
;;; (rect display-to: port)
;;;
;;; Displays the textual representation of a rectangle to the given IO port.

(let ((method
        (lambda (self port)
            (display-to port "#<rectangle ")
            (display-to port (self : <rectangle> x)) (display-to port " ")
            (display-to port (self : <rectangle> y)) (display-to port " ")
            (display-to port (self : <rectangle> w)) (display-to port " ")
            (display-to port (self : <rectangle> h))
            (display-to port ">"))))
    (<rectangle> add-method: 'write-to: method)
    (<rectangle> add-method: 'display-to: method))

;;; (rect contains? x y)
;;;
;;; Returns true if the rectangle contains the point (x, y); otherwise
;;; returns false.

(define-method <rectangle> (self contains? x y)
    (and (>= x (self : <rectangle> x))
         (>= y (self : <rectangle> y))
         (<= (- x (self : <rectangle> x)) (self : <rectangle> w))
         (<= (- y (self : <rectangle> y)) (self : <rectangle> h))))

;;; (min a b)
;;; (max a b)
;;;
;;; Return the minimum/maximum of the two arguments.

(define (min a b)
    (if (< a b) a b))

(define (max a b)
    (if (> a b) a b))

;;; (rect intersect: rect2)
;;;
;;; Returns either a new rectangle, which is the area common to both arguments,
;;; or #f if there is no overlap.

(define-method <rectangle> (self intersect: other)
    (let ((result (<rectangle> new:
                    (max (self : <rectangle> x) (other : <rectangle> x))
                    (max (self : <rectangle> y) (other : <rectangle> y))
                    (min (self x2) (other x2))
                    (min (self y2) (other y2)))))
        (result set: <rectangle> w
            (- (result : <rectangle> w) (result : <rectangle> x)))
        (result set: <rectangle> h
            (- (result : <rectangle> h) (result : <rectangle> y)))
        (if (or (<= (result : <rectangle> w) 0)
                (<= (result : <rectangle> h) 0))
            #f
            result)))

;;; (rect not)
;;;
;;; Returns a list of rectangles, which when taken together cover all of a
;;; 640x480 screen excluding the area covered by the argument.

(define-method <rectangle> (self not)
    (list
        (<rectangle> new: 0                       (self : <rectangle> y)
                          (self : <rectangle> x)  (self : <rectangle> h))
        (<rectangle> new: (self x2)               (self : <rectangle> y)
                          (- 640 (self x2))       (self : <rectangle> h))
        (<rectangle> new: 0                       0
                          640                     (self : <rectangle> y))
        (<rectangle> new: 0                       (self y2)
                          640                     (- 480 (self y2))      )))

;;; (rect ->list)
;;;
;;; Returns a list containing x, y, w and h.

(define-method <rectangle> (self ->list)
    (list
        (self : <rectangle> x)
        (self : <rectangle> y)
        (self : <rectangle> w)
        (self : <rectangle> h)))

;;; (rect grow: xdelta ydelta)
;;;
;;; Changes size of rect by adding/subtracting xdelta or ydelta to/from each
;;; coordinate.

(define-method <rectangle> (self grow: x y)
    (self set: <rectangle> x (- (self : <rectangle> x) x))
    (self set: <rectangle> y (- (self : <rectangle> y) y))
    (self set: <rectangle> w (+ (self : <rectangle> w) (* x 2)))
    (self set: <rectangle> h (+ (self : <rectangle> h) (* y 2))))

;;; (rect move: xdelta ydelta)
;;;
;;; Changes position of rect by adding xdelta or ydelta to each coordinate.

(define-method <rectangle> (self move: x y)
    (self set: <rectangle> x (+ (self : <rectangle> x) x))
    (self set: <rectangle> y (+ (self : <rectangle> y) y)))

;;; (rect copy)
;;;
;;; Returns a new <rectangle> identical to the argument.

(define-method <rectangle> (self copy)
    (apply ((self class) get-class-method: 'new:) (self class) (self ->list)))

;;; (rect top-left)
;;; (rect bottom-right)
;;;
;;; Return coordinates of the corner requested.

(define-method <rectangle> (self top-left)
    (list
        (self : <rectangle> x)
        (self : <rectangle> y)))

(define-method <rectangle> (self bottom-right)
    (list
        (self x2)
        (self y2)))

;----------------------------------------------------------------------------;

;;; Class <view>
;;;
;;; Abstract windowable (displayable) object.
;;; Instance variables:
;;;     bounds      The rectangle representing the area of the screen covered
;;;                 by the view.
;;;     owner       The view under which this view is logically grouped.
;;;     children    Views logically grouped under this view.

(define-class <view> <object> (bounds owner children))

;;; (<view> new: x y w h owner)
;;;
;;; Creates and returns a view with the given coordinates, and the given
;;; logical owning view.

(define-class-method <view> (self new: x y w h owner)
    (let ((n (self new)))
        (n set: <view> bounds (<rectangle> new: x y w h))
        (n set: <view> children '())
        (unless (null? owner)
            (owner add-child: n))
        n))

;;; (view destroy)
;;;
;;; Cleans up as a view is removing itself from the windowing system.

(define-method <view> (self destroy)
    (unless (null? (self : <view> owner))
        ((self : <view> owner) remove-child: self)))

;;; (view has-focus?)
;;;
;;; Returns true if this view has the input focus.

(define-method <view> (self has-focus?)
    (eq? (desktop : <desktop> focus) self))

;;; (view paint: rect)
;;;
;;; Sets up the graphics system to draw stuff in the area specified by rect.

(define-method <view> (self paint: area)
    (set! clipping-rectangle (area ->list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (view draw: rect)
;;;
;;; Repaints the areas of view and its children which are contained within the
;;; region of the screen represented by rect.
;;;
;;; WARNING: The primitive form of this method relies on the structure of the
;;; classes <view> and <rectangle>. Be careful when changing those classes to
;;; also update the information used by this method.

(<view> add-method: 'draw: %%draw-method-for-<view>-objects)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (view add-child: child)
;;; (view remove-child: child)
;;;
;;; Add or remove child views from a view.

(define-method <view> (self add-child: child)
    (unless (member child (self : <view> children))
        (child set: <view> owner self)
        (self set: <view> children
            (cons child (self : <view> children)))))

(define-method <view> (self remove-child: child)
    (let ((l (member child (self : <view> children))))
        (when l
            (child set: <view> owner '())
            (if (null? (cdr l))
                (set-car! l #f)
                (begin
                    (set-car! l (cadr l))
                    (set-cdr! l (cddr l)))))))

;----------------------------------------------------------------------------;

;;; Class <desktop>
;;;
;;; The root view in the windowing system is an instance of class <desktop>.

(define-class <desktop> <view> (focus))

;;; (<desktop> new)
;;;
;;; Creates a new desktop, setting its logical boundaries to the screen size.

(define-class-method <desktop> (self new)
    (let ((n (self as: <view> new)))
        (n set: <view> bounds
            (<rectangle> new: 0 0 640 480))
        (n set: <desktop> focus '())
        n))

;;; (desktop paint: area)
;;;
;;; Redraws areas of the desktop.

(define-method <desktop> (self paint: area)
    (self as: <view> paint: area)
    (apply fill-rect grey-25 3 ((self : <view> bounds) ->list)))

;;; (desktop draw)
;;;
;;; Refresh the entire screen.

(define-method <desktop> (self draw)
    (self draw: (self : <view> bounds)))

;----------------------------------------------------------------------------;

;;; Class <window>
;;;
;;; This is pretty self-evident :-)

(define-class <window> <view> (title flags))

;;; (<window> new: x y w h owner title)
;;;
;;; Creates and returns a window with the specified attributes.

(define-class-method <window> (self new: x y w h owner title)
    (let ((n (self as: <view> new: x y w h owner)))
        (n set: <window> title title)
        n))

;;; (window paint: area)
;;;
;;; Refresh areas of the window.

(define-method <window> (self paint: area)
    (self as: <view> paint: area)
    (let ((bounds ((self : <view> bounds) copy)))
        (apply fill-rect '() 15 (bounds ->list))
        (apply rectangle 0 2 (bounds ->list))
        (bounds grow: -2 -2)
        (bounds set: <rectangle> h (+ (gui-font : <font> height) 2))
        (screen-form print-string:
            (self : <window> title)
            (+ (bounds : <rectangle> x) 2)
            (+ (bounds : <rectangle> y) 2)
            gui-font 4)
        (apply BitBlt screen-form '() '() 0
                      (bounds : <rectangle> x) (bounds y2)
                      (bounds : <rectangle> w) 2
                      (append clipping-rectangle '(0 0)))
        (if (self has-focus?)
            (apply BitBlt screen-form '() '() 10
                          (append (bounds ->list)
                                  clipping-rectangle
                                  '(0 0)))
            (apply BitBlt screen-form '() grey-25 4
                          (append (bounds ->list)
                                  clipping-rectangle
                                  '(0 0))))))

;----------------------------------------------------------------------------;

;;; desktop
;;;
;;; The system-wide desktop.

(define desktop (<desktop> new))

(desktop add-child: (<window> new: 100 100 (- 200 100) (- 200 100) desktop "Title 2"))
(desktop add-child: (<window> new: 370 300 (- 620 370) (- 460 300) desktop "Title 3"))
(desktop add-child: (<window> new: 320 50 (- 420 320) (- 450 50) desktop "Title 4"))
(desktop add-child: (<window> new: 120 160 (- 500 120) (- 400 160) desktop "Title 5"))
(desktop add-child: (<window> new: 140 120 (- 400 140) (- 300 120) desktop "Title 9"))
(desktop add-child: (<window> new: 50 140 (- 550 50) (- 350 140) desktop "Title 7"))
(desktop add-child: (<window> new: 150 150 (- 250 150) (- 250 150) desktop "Title 14"))

(define gui-font smalthin-font) ;modernb-font)

(define (test-gui)
    (graphics-mode)
    (desktop draw)
    (let ((background (<form> new: 8 16)))
        (let loop ((state (get-mouse)))
            (let ((x (list-ref state 0))
                  (y (list-ref state 1))
                  (b (list-ref state 2)))
                (unless (= b 7)
                    (if (= b 3)
                        (desktop draw))
                    (set! clipping-rectangle '(0 0 640 480))
                    (BitBlt background screen-form '() 3
                            0 0 8 16
                            0 0 8 16
                            x y)
                    (screen-form print-string: "" x y system-font 4)
                    (BitBlt screen-form background '() 3
                            x y 8 16
                            0 0 640 480
                            0 0)
                    (loop (get-mouse))))))
    (text-mode))

