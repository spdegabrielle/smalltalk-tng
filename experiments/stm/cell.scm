;;
;; Issues remaining:
;;
;; - pending-thunks need to be able to remove their thread from all cells pended-upon
;; - retries need to merge transaction-logs in proper nested fashion
;; - need to implement orelse
;;

(define-record-type cell
  (make-cell* owner pending value)
  cell?
  (owner cell-owner set-cell-owner!)
  (pending cell-pending set-cell-pending!)
  (value cell-value set-cell-value!))

(define-record-type transaction
  (make-transaction* parent restart abort-catcher log)
  transaction?
  (parent transaction-parent)
  (restart transaction-restart)
  (abort-catcher transaction-abort-catcher)
  (log transaction-log set-transaction-log!))

(define-record-type log-entry
  (make-log-entry cell previous-owner initial-value next)
  log-entry?
  (cell log-entry-cell)
  (previous-owner log-entry-previous-owner)
  (initial-value log-entry-initial-value)
  (next log-entry-next))

;---------------------------------------------------------------------------

(define (make-cell initial-value)
  (make-cell* #f '() initial-value))

;---------------------------------------------------------------------------

(define tasklist '())

(define (spawn thunk)
  ...)

;---------------------------------------------------------------------------

(define current-transaction (make-parameter #f))

(define (require-transaction where)
  (or (current-transaction)
      (error "transaction required" where)))

(define (cell-unlocked? cell requesting-transaction)
  (let ((owner (cell-owner cell)))
    (or (not owner)
	(let loop ((txn requesting-transaction))
	  (cond ((not txn) #f)
		((eq? txn owner) #t)
		(else (loop (transaction-parent txn))))))))

(define (access-cell! cell action-thunk)
  (let ((transaction (require-transaction 'access-cell!))
	(previous-owner (cell-owner cell)))
    (cond
     ((eq? transaction previous-owner) (action-thunk))
     ((cell-unlocked? cell transaction)
      (let ((new-log (make-log-entry cell
				     previous-owner
				     (cell-value cell)
				     (transaction-log transaction))))
	(set-transaction-log! transaction new-log)
	(set-cell-owner! cell transaction)
	(action-thunk)))
     (else
      (internal-retry (cons cell (map log-entry-cell (transaction-log transaction))))))))

(define (get-cell cell)
  (access-cell! cell (lambda () (cell-value cell))))

(define (set-cell! cell value)
  (access-cell! cell (lambda () (set-cell-value! cell value))))

(define (retry)
  (internal-retry (map log-entry-cell (transaction-log (require-transaction 'retry)))))

(define (abort-transaction result)
  ((transaction-abort-catcher (require-transaction 'abort-transaction)) result)
  ;; Paranoia:
  (error "transaction-abort-catcher returned!"))

(define (internal-retry accessed-cells)
  ...)

(define (with-transaction thunk)
  (call-with-current-continuation
   (lambda (return-from-transaction)
     (let* ((abort-transaction (lambda (result)
				 ...))
	    (restart (lambda ()
		       ...))
	    (transaction (make-transaction* (current-transaction)
					    restart
					    abort-transaction
					    #f)))
       (parameterize ((current-transaction transaction))
	 (let ((result (thunk)))
	   ...
	   result))))))
