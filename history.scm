(define *history* #t)

(define *max-history* 1000)

(define *history-count* 0)

(define *history-holder* '())

(define *history-rules-inclusive* #f)

(define *history-rules* '())

(set! *history-rules* (cons (lambda (txt)
			      #t)
			    *history-rules*))

(define (add-history-item text)
  ;; (display "adding ") (display-color 2 text) (echo " to history")
  ;; (echo "adding to history")
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
  ;; (set! *history-count* (+ *history-count* 1))
  ;; (set! *history-holder* (cons text *history-holder*))
  ;; (echo "added to history")
  (if (or (equal? *history-count* *max-history*)
	  (> *history-count* *max-history*))
      (begin ;; (echo "in if true")
	     (set! *history-holder* (list-head *history-holder*
				      (- *max-history* 1)))
	     (set! *history-count* (- *max-history* 1)))))

(define (history-previous-entry . l)
  (if (null? l)
      (second *history-holder*)
      (let ((contents (cdr *history-holder*)))
	(list-ref contents (car l)))))
