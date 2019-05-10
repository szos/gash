# GASH

GASH is in very early development. commands may change - the api shouldnt be considered stable.

GASH is a shell based on guile scheme. However, its not exactly a shell. Its more of an interpretation layer. The main purpose of GASH is to allow scheme code to be spliced in, via §. for example: `$ echo '§(format #f "welcome, ~a! the pwd is ~a" user-at-host (getcwd))'` This would be parsed, and turned into `echo 'welcome, my-username@my-computer! the pwd is /home/my-username'` before being fed into sh. We send it into sh, but we could just as easily write shell functionalities in c, and use those instead.

## Why Should I Care and/or Use GASH?

Because its FUN! For basic shell usage, it works just fine, same as bash, except for completions following an opening paren. It lets you use scheme from the command line with a simple switch character to either things for side effects or splicing in text. The only place it really falls flat on its face is in completions on things with spaces in them. 

## Installation

Clone this repo, and ensure guile is installed to /usr/bin/guile. CD into the GASH directory and run ./configure. This will build libgash.so and set gash to be executable. To install, run `./configure --install`. This will install GASH to /usr/local/bin/gash and the libraries to /usr/local/lib/. This will put GASH into the PATH, allowing you to run it anywhere. 

## Usage

GASH has several functionalities. Firstly, text is read in via gnu readline. Next, the scheme sections(§) are replaced. Following that, the string is split into tokens. At this point we handle builtins, which you can define. Then user defined shorthand is replaced, and finally, its executed via /bin/sh (which is sometimes symlinked to bash). If the string begins with an opening paren, it is treated as a scheme string, and its tokenized, shorthand replaced, and joined back up before being passed to guile. 

GASH reads from a user directory, ~/.gashrc, where one can define whatever is needed. As GASH uses readline, ~/.inputrc is also read and respected.

To change the character to switch to scheme to a comma, place `(set! scheme-switch #\,)` in you ~/.gashrc. 

### §(...)

Conceptually, this is rather simple. If the toplevel form following § is a string, the § and form is replaced by its evaluation. Otherwise, it is assumed to be run for the side effects, in which case it is replaced by an empty string.

### Builtins

GASH handles builtins a little funkily. This is due to GASH parsing a string and then passing it off to sh, instead of forking and exec-ing on its own. Builtins are only recognized if they're in the first position.

The builtins are held in an alist, and consist of a name as a string and a lambda, which takes a list, and returns a list.
One can define a builtin like so: `(define-builtin "my-builtin" (lambda (lst) (my-func) '()))`.
Regarding the lambda; The lambda will be called with the full command line, tokenized, and must return anything that they want executed, as a tokenized list. For example, if I defined some builtin that changed the current directory, I might return `'("ls" "--color" "-a")` in order to list the new current directory.

### Shorthand

Shorthand is like the alias of GASH. The last step before un-tokenizing is to replace any shorthand with its longhand. We check every token against the list of shorthand the user has defined, and whenever we encounter one that matches it is replaced with its appropriate longhand. There is one predefined shorthand, which changes any instance of "ls" with "ls --color". If a user wants to rid themselves of this behaviore, they can run `(define-shorthand "ls" "ls")` to reset it.

## Completions

GASH doesnt use the regular guile readline extension, instead building a similar system within libgash.c. This allows us to be more hands on with completions. Currently, we have a set of rules for our default completions in the variable default-gash-completer. The earlier a rule occurs in the cond statement, the higher priority it is. eg if you have two conflicting rules, the first one to match is the one used. So, if a word begins with our switch followed by an opening paren, or just a paren, we use guiles apropos completion function. Otherwise, if were at the start of the line (ie working on the first word) we complete based on commands, and finally we otherwise complete based on the default file completion function. 

## Wanted Features

There are a couple features we will be wanting in GASH. The following is an unordered list of wanted features. If you have an idea for implementing any of them, open a pull request.
1. More meaningful shell interaction. Write better interaction functions between shell and guile. Since the § has augmented pipes so much, it seems only right to have a $, which runs a shell command, collecting its output. This functionality is already implemented in gash-base-lib as collect-shell-command, but could be improved and gussied up. For example, send the string send in through our parser for guile, in a mutually recursive manner, so we can do `§(fun $[echo '§(otherfun $[ls -al])'])` and have everything be expanded propperly. The prior syntax is only a suggestion. 

## Issues and Quirks

There are some issues with readline, specifically with reading over a certain length, and skipping back to the beginning of the line with C-a. I'm no C wizard, and theres probably things that could be done I dont know of. I think looking to the Bash usage of readline would be a good start.

There are a couple quirks. Firstly, as GASH accessorizes the shell, and isnt itself a shell, there end up being some oddities with running it as your default shell. Since its not technically a shell, its not in /etc/shells, so you cant just set it as a default. Another way of launching might be to run `xterm -e /path/to/gash`, but this doesnt work either. The only way of launching, currently, is to open a terminal and run it from there.

Another quirk is strings. When you switch to scheme mode, and call a shell command (generally via (sh "...")) you need to make sure to escape strings. Take this example call: `$ echo '§(format #f "~a" (sh "ls §(format #f "/home")"))'` This call will error out, due to misquoted strings. Because the inner format contains a string while being contained within a string, the inner string needs to be escaped, like so: `$ echo '§(format #f "~a" (sh "ls §(format #f \"/home\")"))'`.
Of course, this is all moot as sh and § dont play well yet. 