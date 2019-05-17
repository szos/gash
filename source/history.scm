(define *history* #t)

(define *max-history* 1000)

(define *history-count* 0)

(define *history-holder* '())

(define *history-rules-inclusive* #f)

(define *history-rules* '())

(define (define-history-rule function)
  "adds a lamda to the list of lambdas to compute whether or not to add an item 
to the history list. "
  (set! *history-rules* (cons function *history-rules*)))

(define (add-history-item text)
  (let ((list-of-t-f (map (lambda (el)
			    (el text))
			  *history-rules*)))
    (if *history-rules-inclusive*
	(if (member #t list-of-t-f)
	    (begin (set! *history-count* (+ *history-count* 1))
		   (set! *history-holder* (cons text *history-holder*))))
	(if (not (member #f list-of-t-f))
	    (begin (set! *history-count* (+ *history-count* 1))
		   (set! *history-holder* (cons text *history-holder*))))))
  (if (or (equal? *history-count* *max-history*)
	  (> *history-count* *max-history*))
      (begin ;; (echo "in if true")
	     (set! *history-holder* (list-head *history-holder*
				      (- *max-history* 1)))
	     (set! *history-count* (- *max-history* 1)))))

(define (history-previous-entry . l)
  (if (null? l)
      (second *history-holder*)
      (let ((contents (cdr *history-holder*))
	    (num (car l)))
	(if (> num (length contents))
	    ;; (echo-color 1 "")
	    (throw 'out-of-range "Within history-previous-entry: attempted to reference history item that doesnt exist")
	    (list-ref contents num)))))
