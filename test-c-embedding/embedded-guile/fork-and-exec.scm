(define (fork-and-execute . args)
  ;; (echo "beginning things...")
  (fork-and-execute-a-list args))

(define (split-for this within . reinstate?)
  "takes a thing to split for and a thing to split in, and if 'this' doesnt 
occur, we return 'within', but if it does, we return a list of all the things. 
if reinstate? is provided (i.e. not null) we return a list of all the things with
'this' placed in between everything. "
  (if (string-contains ))
  (let looper ((pfx (string->list this))
	       (lst (string->list within))
	       (accum '())
	       (reter '()))
    (cond ((null? lst)
	   (display "null lst\n")
	   (reverse (cons (list->string (reverse accum)) reter)))
	  ((prefix? lst pfx)
	   (display "prefix? is true\n")
	   (looper pfx (list-tail lst (length pfx)) '()
		   (if (null? reinstate?)
		       (cons (list->string (reverse accum)) reter)
		       (cons this (cons (list->string (reverse accum)) reter)))))
	  (else
	   (display "else\n")
	   (looper pfx (cdr lst) (cons (car lst) accum) reter)))))

(define (parse-string-for-shell)
  (let ((text (read-with-completion)))
    (cond ((not (string? text))
	   (supress-next-error)
	   (throw 'prompt-error "Input read is not a string"))
	  ((string=? prompt "")
	   (supress-next-error)
	   (throw 'prompt-error "input read is empty"))
	  ((char=? (string-ref prompt 0) #\()
	   (catch #t
	     (lambda ()
	       (eval-string prompt)
	       (when *history*
		 (add-history-item prompt)))
	     (lambda (key . args)
	       (attempt-scheme-correction prompt key args))))
	  (else
	   ))))

(define (parse-for-and-or text)
  (let* ((ands-of-ors (map (lambda (el)
			     (split-for "&&" el #t))
			   (split-for "||" text #t)))
	 (together (flatten ands-of-ors)))
    ))
