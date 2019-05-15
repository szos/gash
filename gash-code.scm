#!/usr/bin/guile \
-l gash-base-lib -e main -s
!#


;; this let statement is to determine if gash is installed, or if it is already
;; loading extensions and libs. we initially load gash-base-lib, and then we
;; check if a variable placed in by ./conf exists. if it does, we dont need to
;; load anything. otherwise we need to load libgash.so and history.scm from the
;; local directory. 
(let ((extension-path (format #f "~a/libgash.so" (getcwd)))
      (init-function "init_gash_c"))
  (catch 'unbound-variable
    (lambda ()
      (unless header-loads-libgash
	(load-extension extension-path init-function)))
    (lambda (key . args)
      (load-extension extension-path init-function)
      (load "history.scm"))))

(use-modules (ice-9 format)
	     (ice-9 popen)
	     (ice-9 rdelim)
	     (ice-9 textual-ports)
	     (ice-9 session)
	     (ice-9 regex)
	     (ice-9 buffered-input))

(define *suppress-error-messages* #f)

;; (define (with-attempted-completion completer thunk)
;;   (let ((old-comp *readline-alt-completion-function*))
;;     (dynamic-wind
;;       (lambda ()
;; 	(set! *readline-alt-completion-function* completer))
;;       thunk
;;       (lambda ()
;; 	(set! *readline-alt-completion-function* old-comp)))))

;; (load "/usr/local/lib/history.scm")

(define home-directory 
  (let* ((port (open-input-pipe "echo $HOME"))
  	 (dir (read-line port)))
    (close-port port)
    dir))

(define *all-programs*
  (string-split (collect-shell-command "compgen -c" #t) #\newline))

(define user-at-host
  (let* ((user-port (open-input-pipe "echo $USER"))
	 (host-port (open-input-pipe "hostname"))
	 (user (read-line user-port))
	 (host (read-line host-port)))
    (close-port user-port)
    (close-port host-port)
    (format #f "~a@~a" user host)))

(define (directory-contents dir)
  (let ((port (open-input-pipe
	       (if (char=? (string-ref dir 0) #\/)
		   (format #f "ls -a ~a" dir)
		   (format #f "ls -a ~a/~a" (getcwd) dir)))))
    (let gather ((line (read-line port))
		 (accum '()))
      (if (eof-object? line)
	  (begin (close-port port) (reverse accum))
	  (gather (read-line port) (cons line accum))))))

(define (string-app-list string-list)
  (apply string-append string-list))

;;; 

(define scheme-switch #\§)

(define* (walk-string-for-guile string #:optional (location
						   (string-index string scheme-switch)))
  (let ((first-instance location))
    (let loop ((char-list (string->list (substring string (+ 1 first-instance))))
	       (accum '())
	       (paren-counter 0)
	       (length 0))
      ;; (display char-list) (newline)
      (cond ((null? char-list)
	     ;; (echo "null char-list")
	     ;; (if (equal? paren-counter 1)
	     ;; 	 #f)
	     (+ 1 length))
	    ((char=? (car char-list) #\()
	     ;; (echo "found an opening paren")
	     (loop (cdr char-list) (cons (car char-list) accum)
		   (+ paren-counter 1) (+ length 1)))
	    ((char=? (car char-list) #\))
	     ;; (echo "found a closing paren")
	     (loop (cdr char-list) (cons (car char-list) accum)
		   (- paren-counter 1) (+ length 1)))
	    ((equal? paren-counter 0)
	     ;; (echo "paren counter is 0")
	     (+ 1 length))
	    (else
	     (loop (cdr char-list) (cons (car char-list) accum)
		   paren-counter (+ length 1)))))))

(define (extract-scheme-strings string)
  ;; (echo string)
  (if (not (string-index string scheme-switch))
      (list string)
      (let loop ((ref (string-index string scheme-switch))
		 (tail 0)
		 (acc '()))
	;; (echo ref)
	(cond ((not ref)
	       (reverse (cons (substring string tail) acc)))
	      (else
	       (let* ((end-scheme (walk-string-for-guile string ref))
		      (scheme-string
		       (substring string ref
				  (+ ref end-scheme)))
		      (pre-string (substring string tail ref)))
		 (loop (string-index string scheme-switch (+ ref 1))
		       (+ ref end-scheme)
		       (cons scheme-string (cons pre-string acc)))))))))

(define (eval/replace-scheme-string string-list)
  (let loop ((strings string-list)
	     (acc '()))
    (cond ((null? strings)
	   (reverse acc))
	  ((string=? (car strings) "")
	   (loop (cdr strings) (cons (car strings) acc)))
	  ((char=? (string-ref (car strings) 0) scheme-switch)
	   (let ((result
		  (eval-string (substring (car strings) 1))))
	     (loop (cdr strings)
		   (cons
		    (if (string? result)
			result
			"")
		    acc))))
	  (else
	   (loop (cdr strings) (cons (car strings) acc))))))

;;; end guile parsing.

(define (sh . string)
  (let* ((port (open-input-pipe string))
  	 (strs (get-string-all port)))
    (close-port port)
    strs))

(define ($ string)
  (let ((s (collect-shell-command string #t #t)))
    (let ((expanded
	   (string-app-list (eval/replace-scheme-string
			     (extract-scheme-strings s)))))
      expanded))
  ;; (let ((port (open-input-pipe string))
  ;; 	(strs (get-string-all port)))
  ;;   (close-port port)
  ;;   (let ((expanded
  ;; 	   (string-app-list (eval/replace-scheme-string
  ;; 			     (extract-scheme-strings strs)))))
  ;;     expanded))
  )

;; (define-macro ($ string))

(define *builtins-alist*
  `(("cd" . ,(lambda (lst)
	       (cond ((null? (cdr lst))
		      (chdir home-directory)
		      '())
		     ((string=? (second lst) "~/")
		      (begin (chdir home-directory)
			     (cddr lst)))
		     ((string=? (second lst) "")
		      (begin (chdir home-directory)
			     '()))
		     ((directory? (cadr lst))
		      (begin (chdir (cadr lst))
			     (cddr lst)))
		     (else
		      (let cd-loop ((dir-str (second lst))
				    (rem-str (cddr lst)))
			(cond ((null? rem-str)
			       ;; (display "in null? dir-str: ")(echo dir-str)
			       (when (directory? dir-str)
				 (chdir dir-str))
			       '())
			      ((directory? dir-str)
			       ;; (display "in dir? dir-str: ")(echo dir-str)
			       (chdir (cadr lst)) (cdr rem-str))
			      (else
			       ;; (echo dir-str)
			       (cd-loop
				(string-append dir-str " " (car rem-str))
				(cdr rem-str)))))))))
    ("exit" . ,(lambda (lst)
		 (exit 0)))
    ("reload" . ,(lambda (lst)
		   (echo "Loading .gashrc")
		   (load (format #f "~a/.gashrc" home-directory))
		   '()))))

(define (define-builtin name lambda)
  (set! *builtins-alist* (cons `(,name . ,lambda) *builtins-alist*)))

(define *directory-stack* '())

(define-builtin "pudir"
  (lambda (lst)
    (cond ((null? lst) lst)
	  ((null? (cdr lst))
	   (echo-color 1 "Enter a directory")
	   '())
	  ((directory? (second lst))
	   (set! *directory-stack* (cons (getcwd) *directory-stack*))
	   (echo-color 2 (format #f "Pushing from ~a to ~a"
				 (getcwd) (second lst)))
	   (chdir (second lst))
	   (cddr lst))
	  (else
	   (echo-color 1 "unknown directory, try again")
	   (cddr lst)))))

(define-builtin "podir"
  (lambda (lst)
    (cond ((null? lst) lst)
	  ((null? *directory-stack*)
	   (echo-color 1 "Directory stack is empty")
	   (cdr lst))
	  (else
	   (echo-color 2 (format #f "Popping from ~a to ~a"
				 (getcwd) (car *directory-stack*)))
	   (chdir (car *directory-stack*))
	   (set! *directory-stack* (cdr *directory-stack*))
	   (cdr lst)))))

(define *base-shorthand* ; this is for resetting the shorthand alist, should
  '(("ls" . "ls --color"))) ; one put to many entries.

(define *shorthand-alist*
  '(("ls" . "ls --color")))

(define (define-shorthand from to)
  (set! *shorthand-alist* (cons `(,from . ,to) *shorthand-alist*)))

(define (replace-shorthand str-list)
  (let loop ((lst str-list)
	     (accum '()))
    (if (null? lst)
	(reverse accum)
	;; (join-strings  " ")
	(let ((shorthand? (assoc (car lst) *shorthand-alist*)))
	  (if shorthand?
	      (loop (cdr lst) (cons (cdr shorthand?) accum))
	      (loop (cdr lst) (cons (car lst) accum)))))))

(define (generate-prompt)
  (let ((curdir (getcwd)))
    (if (string=? curdir home-directory)
	(format #f "~a ~a" user-at-host "~")
	(let ((p (substring curdir (+ 1 (string-index-right curdir #\/)))))
	  (format #f "~a ~a" user-at-host p)))))

(define commands-completion-function
  (let ((completions '()))
    (lambda (text cont?)
      (if (not cont?)
	  (set! completions
	    (let loop ((cmp *all-programs*)
		       (accum '()))
	      (if (null? cmp)
		  accum
		  (if (string-prefix? text (car cmp))
		      (loop (cdr cmp) (cons (car cmp) accum))
		      (loop (cdr cmp) accum))))))
      (if (null? completions)
          #f
          (let ((retval (car completions)))
            (begin (set! completions (cdr completions))
                   retval))))))

(define apropos-completion-function-test
  (let ((completions '()))
    (lambda (text cont?)
      ;; (echo "in apropos scheme function")
      (if (not cont?)
	  (set! completions
	    (map symbol->string
		 (apropos-internal
		  (string-append "^" (regexp-quote text))))))
      (if (null? completions)
	  #f
	  (let ((retval (car completions)))
	    ;; (echo "completions not null, and state not #f")
	    (begin (set! completions (cdr completions))
		   retval))))))

(define-macro (make-completer . body)
  "This takes a body, which can reference the variable current-word. This is 
more useful than just having text. text is divided based on many things, so if 
one types §(for and presses tab, it will complete based on for, as the §( is 
treated as a seperate thing. so we have 4 total things we can reference:
start and end, which are our bounds within (get-line-contents), as well as
text and current-word, which are our current completion text, and the last word
in the line (meaning the final space to the end of the line)"
  `(lambda (text start end)
     (let ((current-word (last (string-split (get-line-contents) #\space))))
       ,@body)))

(define default-gash-begin-scheme-string
  (format #f "~a(" scheme-switch))

(define default-gash-completer
  (make-completer
   (cond ((string-prefix? default-gash-begin-scheme-string current-word)
	  (set! *readline-generator-function* apropos-completion-function-test)
	  #t)
	 ((string-prefix? "(" current-word)
	  (set! *readline-generator-function* apropos-completion-function-test)
	  #t)
	 ((equal? start 0)
	  (set! *readline-generator-function* commands-completion-function)
	  #t)
	 (else #f))))

(define (completer-switch text start end)
  (let* ((line (get-line-contents))
	 (split-line (string-split line #\space))
	 (current-word (last split-line)))
    (cond ((string-prefix? "§(" current-word)
	   (set! *readline-generator-function* apropos-completion-function-test)
	   #t)
	  ((string-prefix? "(" current-word)
	   (set! *readline-generator-function* apropos-completion-function-test)
	   #t)
	  ((equal? start 0)
	   (set! *readline-generator-function* commands-completion-function)
	   #t)
	  (else #f))))

(define (completer-for-alt text start end)
  "return #f to indicate we want to return NULL in the c function and 
subsequently use filename completion. return #t to call the 
*readline-generator-function* that MUST be defined. if we want to switch 
between readline generator functions then EVERY condition MUST set up its 
own readline function, or return #f.  "
  ;; (echo "in completer-for-alt")
  (cond ((equal? start 0)
  	 ;; (echo "start is 0")
	 (echo (get-line-contents))
         (set! *readline-generator-function* commands-completion-function)
  	 #t)
	;; ((string=? text "")
	;;  #f)
	;; ((string=? "§(" text)
	;;  (echo text) #t)
  	((string-prefix? "§(" text)
  	 (set! *readline-generator-function* apropos-completion-function-test)
	 ;; (echo "readline-generator-function is apropos function")
	 (echo (get-line-contents))
  	 #t)
  	(else #f)))

(define (read-test)
  (read-with-prompt (generate-prompt)))

(define (attempt-scheme-correction text key args)
  (display-color 9 "Error in evaluation!\n")
  (display-color 9 "Error Key:  ") ; (format #f "Error Key:  ~a\n" key)
  (display key) (newline)
  (display-color 3 "Error Args:  ") ; (format #f "Error Args:  ~a\n" args)
  (display args) (newline)
  (display-color 2 "attempted evaluation: ") (display text)
  (newline)
  (display-color 3 "enter corrected sexp: ")
  (let ((fixed-sexp (read-line)))
    (cond ((or (string=? fixed-sexp "quit")
	       (string=? fixed-sexp "exit")
	       (string=? fixed-sexp "break")
	       (string=? fixed-sexp ""))
	   (throw 'exited-correction "exited correction attempt "))
	  (else
	   (catch #t
	     (lambda ()
	       (eval-string fixed-sexp)
	       (when *history*
		 (add-history-item prompt)))
	     (lambda (key . args)
	       ;; (display-color 9 "Evaluation Failed. Again. ")
	       (throw 'correction-failure
		      "The corrected text also failed. ")))))))

(define (read-attempt-comp . prompt)
  ;; (set! *readline-alt-completion-function* completer-for-alt)
  ;; (set! *readline-alt-completion-function* completer-switch)
  (set! *readline-alt-completion-function* default-gash-completer)
  (read-with-prompt (if (null? prompt)
			(generate-prompt)
			(car prompt))))

(define (read-and-interpret)
  ;; (echo "main-loop")
  (let ((prompt ;; (get-string-from-user)
	 ;; (read-with-prompt (generate-prompt))
	 (read-attempt-comp)
	 ))
    ;; (display "prompted text: ") (echo prompt)
    (cond ((not (string? prompt))
	   (read-and-interpret))
	  ((string=? prompt "")
	   (read-and-interpret))
	  ((char=? (string-ref prompt 0) #\()
	   (catch #t
	     (lambda ()
	       (eval-string prompt)
	       (when *history*
		 (add-history-item prompt)))
	     (lambda (key . args)
	       (attempt-scheme-correction prompt key args)
	       ;; (display-color 9 "Error in evaluation!\n")
	       ;; (display-color 9 (format #f "Error Key:  ~a\n" key)) 
	       ;; (display-color 3 (format #f "Error Args:  ~a\n" args))
	       ;; (display-color 2 "attempted evaluation: ") (display prompt)
	       ;; (newline)
	       ;; (display-color 3 "enter corrected sexp: ")
	       ;; (let ((fixed-sexp (read-line)))
	       ;; 	 (cond ((or (string=? fixed-sexp "quit")
	       ;; 		    (string=? fixed-sexp "exit")
	       ;; 		    (string=? fixed-sexp "break")
	       ;; 		    (string=? fixed-sexp ""))
	       ;; 		(throw 'exited-correction "exited correction attempt "))
	       ;; 	       (else
	       ;; 		(catch #t
	       ;; 		  (lambda () (eval-string fixed-sexp))
	       ;; 		  (lambda (key . args)
	       ;; 		    (display-color 9 "Evaluation Failed. Again. ")
	       ;; 		    (throw 'correction-failure
	       ;; 			   "The corrected text also failed. "))))))
	       )))
	  (else
	   (when *history*
	     (add-history-item prompt))
	   (let* ((scheme-replaced
		   (string-app-list
		    (eval/replace-scheme-string
		     (extract-scheme-strings prompt))))
		  (tokens (string-split scheme-replaced #\space))
		  (func (assoc (car tokens)
			       *builtins-alist*)))
	     ;; (display "scheme replaced string: ")(echo scheme-replaced)
	     (if func ;; if the first element is a builtin
		 (let ((strs ((cdr func) tokens)))
		   (unless (null? strs)
		     (system (join-strings (replace-shorthand strs) " "))))
		 (let ((longhand (replace-shorthand tokens)))
		   (system (join-strings longhand " ")))))))))

(define (loop-handler)
  (catch #t
    (lambda ()
      (while 1
	(read-and-interpret)))
    (lambda (key . args)
      (cond ((equal? key 'quit)
	     (exit))
	    (else
	     (unless *suppress-error-messages*
	       (display-color 1 "ERROR:  ")
	       (display key) (display ", \n")
	       (display-color 9 "REPORT:  ")
	       (display (car args))
	       (map (lambda (el) (display ", ") (display el)) (cdr args))
	       ;; (newline)
	       ;; (display (format args))
	       )
	     ;; (echo args)
	     (newline)
	     (loop-handler))))))

(define builtins-list-for-completions
  ;; complete for builtins as well in the first position.
  (map (lambda (el)
	 (car el))
       *builtins-alist*))

(define (main . args)
  (load (string-append home-directory "/.gashrc"))
  (display  "loaded ") (echo (string-append home-directory "/.gashrc"))
  (set! *all-programs* (append builtins-list-for-completions *all-programs*))
  ;; (let ((port (open-file "gash.log" "w")))
  ;;   (display "GASH logfile" port) (newline port) (newline port)
  ;;   (close-port port))
  (loop-handler))


