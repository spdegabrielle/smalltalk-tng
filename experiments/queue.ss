;;; <queue.ss> ---- Excessively simple queue implementation.
;;; Copyright (C) 2004 by Tony Garnock-Jones.

;;; This is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.

;;; This software is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this software; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA

;;; Author: Tony Garnock-Jones <tonyg@kcbbs.gen.nz>

(module queue mzscheme
  (provide make-q enq! deq! q->list list->q q-empty?)

  (define (make-q)
    (cons '() '()))

  (define (enq! q e)
    (let ((cell (cons e '())))
      (if (null? (car q))
	  (set-car! q cell)
	  (set-cdr! (cdr q) cell))
      (set-cdr! q cell)))

  (define (deq! q)
    (if (null? (car q))
	#f
	(let ((v (caar q)))
	  (set-car! q (cdar q))
	  (if (null? (car q))
	      (set-cdr! q '()))
	  v)))

  (define (q->list q)
    (car q))

  (define (list->q lst)
    (let ((q (make-q)))
      (for-each (lambda (x) (enq! q x)) lst)
      q))

  (define (q-empty? q)
    (null? (car q)))
)
