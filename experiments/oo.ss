;;; OO.SS   Support for object-oriented programming

(let ((cell (assq 'add-method: (<class> : <class> methods))))
    (let ((oldmeth (cdr cell)))
        (set-cdr! cell
            (lambda (self name func)
                (cond
                    ((assq name (self : <class> methods)) =>
                        (lambda (cell)
                            (set-cdr! cell func)
                            self))
                    (else
                        (oldmeth self name func)))))))

(define-method <class> (self add-class-method: name lambda)
    (self set: <class> class-methods
        (cons (cons name lambda)
              (self : <class> class-methods))))

(define define-class-method
    (macro (class template . body)
        `(,class add-class-method: ',(cadr template)
            (lambda ,(cons (car template) (cddr template))
                ,@body))))

(define-class-method <class> (self new: super ivars print-name)
    (let ((new (self new)))
        (new set: <class> super super)
        (new set: <class> ivars ivars)
        (new set: <class> numivars
            (+ (super : <class> numivars) (length ivars)))
        (new set: <class> print-name print-name)
        new))

; (define-class name super (ivar1 ...))

(define define-class
    (macro (name super ivars)
        `(define ,name (<class> new: ,super ',ivars ',name))))

; (class get-method: name)

(define-method <class> (self get-method: name)
    (let ((methods (self : <class> methods)))
        (cond
            ((assq name methods) => cdr)
            (else #f))))

; (class get-class-method: name)

(define-method <class> (self get-class-method: name)
    (let ((methods (self : <class> class-methods)))
        (cond
            ((assq name methods) => cdr)
            (else #f))))

(define-method <class> (self write-to: port)
    (display "#<class ")
    (display (self : <class> print-name))
    (display ">"))

(define-method <class> (self display-to: port)
    (display "#<class ")
    (display (self : <class> print-name))
    (display ">"))

(define-method <object> (self dissect-to: port)
    (for-each (lambda (x) (display-to port x))
        (list
            "An instance of class " ((self class) : <class> print-name) ".\n"
            "Instance variables:\n"))
    (let loop ((class (self class)))
        (unless (or (null? class) (eq? class <object>))
            (display-to port "--------")
            (display-to port (class : <class> print-name))
            (display-to port "\n")
            (for-each (lambda (ivar)
                            (display-to port "\t")
                            (display-to port ivar)
                            (display-to port "\t\t")
                            (if (and (eq? class <class>)
                                     (memq ivar '(methods class-methods)))
                                (display-to port
                                    (map car
                                        (self get-ivar-by-name: ivar class)))
                                (display-to port
                                    (self get-ivar-by-name: ivar class)))
                            (display-to port "\n"))
                (class : <class> ivars))
            (loop (class : <class> super)))))

(define-method <object> (self dissect)
    (self dissect-to: %%stdout))

(define-method <object> (self instance-of? class)
    (let loop ((c (self class)))
        (cond
            ((null? c) #f)
            ((eq? c class) #t)
            (else
                (loop (c : <class> super))))))

