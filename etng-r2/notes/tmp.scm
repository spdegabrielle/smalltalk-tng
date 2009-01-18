(define (folding-map f acc l)
  (cond
   ((null? l) '())
   ((pair? l)
    (let ((new-acc (f acc (car l))))
      (cons new-acc
	    (folding-map f new-acc (cdr l)))))))

(define (reduce-map f l)
  (cond
   ((null? l) '())
   ((pair? l) (cons (car l) (folding-map f (car l) (cdr l))))))
