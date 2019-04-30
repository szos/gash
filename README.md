# GASH

GASH is in very early development. commands may change - the api shouldnt be considered stable.

GASH is a shell based on guile scheme. However, its not exactly a shell. Its more of an interpretation layer. The main purpose of GASH is to allow scheme code to be spliced in, via §. for example: `$ echo '§(format #f "welcome, ~a! the pwd is ~a" user-at-host (getcwd))'` This would be parsed, and turned into `echo 'welcome, my-username@my-computer! the pwd is /home/my-username'` before being fed into sh. We send it into sh, but we could just as easily write shell functionalities in c, and use those instead.

## Installation

Clone this repo, and ensure guile is installed to /usr/bin/guile. Now `cd` into the GASH directory and run `$ chmod +x gash`. Run `make` to generate the neccessary .so file for GASH. A more permanent installation isnt available currently.

## Usage

GASH has several functionalities. Firstly, text is read in via gnu readline. Next, the scheme sections(§) are replaced. Following that, the string is split into tokens. At this point we handle builtins, which you can define. Then user defined shorthand is replaced, and finally, its executed via /bin/sh (which is sometimes symlinked to bash).

### §(...)

Conceptually, this is rather simple. If the toplevel form following § is a string, the § and form is replaced by its evaluation. Otherwise, it is assumed to be run for the side effects, in which case it is replaced by an empty string.

### Builtins

GASH handles builtins a little funkily. This is due to GASH parsing a string and then passing it off to sh, instead of forking and exec-ing on its own. Builtins are only recognized if they're in the first position.

The builtins are held in an alist, and consist of a name as a string and a lambda, which takes a list, and returns a list.
One can define a builtin like so: `(define-builtin "my-builtin" (lambda (lst) (my-func) '()))`.
Regarding the lambda; The lambda will be called with the full command line, tokenized, and must return anything that they want executed, as a tokenized list. For example, if I defined some builtin that changed the current directory, I might return `'("ls" "--color" "-a")` in order to list the new current directory.

### Shorthand

Shorthand is like the alias of GASH. The last step before un-tokenizing is to replace any shorthand with its longhand. We check every token against the list of shorthand the user has defined, and whenever we encounter one that matches it is replaced with its appropriate longhand. There is one predefined shorthand, which changes any instance of "ls" with "ls --color". If a user wants to rid themselves of this behaviore, they can run `(define-shorthand "ls" "ls")` to reset it. 