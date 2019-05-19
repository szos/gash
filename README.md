# GASH

GASH is in very early development. commands may change - the api shouldnt be considered stable.

GASH is a shell based on guile scheme. However, its not exactly a shell. Its more of an interpretation layer. The main purpose of GASH is to allow scheme code to be spliced in, via §. for example: `$ echo '§(format #f "welcome, ~a! the pwd is ~a" user-at-host (getcwd))'` This would be parsed, and turned into `echo 'welcome, my-username@my-computer! the pwd is /home/my-username'` before being fed into sh. We send it into sh, but we could just as easily write shell functionalities in c, and use those instead.

## Why Should I Care and/or Use GASH?

Because its FUN! For basic shell usage, it works just fine, same as bash, except for completions following an opening paren complete guile. It lets you use scheme from the command line with a simple switch character to either things for side effects or splicing in text. The only place it really falls flat on its face is in completions on things with spaces in them.

### An Example of GASH Extensibility

Say we want to move up a directory and do something. Currently, we would write `cd .. somthing --to 'do'`, but we want to make it more clear by using `&&`. We will use `ls` as a standin for the command following cd. 

## Installation

Clone this repo, and ensure guile is installed to /usr/bin/guile. CD into the GASH directory. What you do depends on how you want to install GASH; If you just want to try it out, make gash-code.scm executable and run it. If you want to install it, heres some example commands:
`./generate --install` this will assemble gash together and install it, and load the required libraries from the current directory. If this directory is deleted it will fail to load.

`./generate --local no --install` This will do the same as above, but install the libraries as well. 

here are the flags and what they do:

Commands to use/install gash:
* --generate; --generate builds `libgash.so` and assembles gash-code.scm and history.scm into a single file which is then set to be executable. If --local is passed, it must be before --generate.
* --install; --install does the same thing as --generate except it also installs the file made by --generate to `execdir`. if --local is passed, it must be passed before --install. First, we copy `single-file-name` to `/execdir/exececutable-name`. If local is true,thats all we do, otherwise we also copy our libraries to `libsdir`.

Commands to control how we generate and install gash:
* --local; --local should be passed to control the whether we want things to load from the git directory, or we want to load things from `libsdir`. It should also pass either a `y` or an `n` to determine whether or not to load from `libsdir`. If we dont pass local, it is assumed to be true. This flag controls the `local?` variable. 
* --set-libsdir; this sets `libsdir` to the string following the flag. `libsdir` is the path to where our libraries will be put and loaded from if `local?` is false.
* --set-execdir; this sets `execdir`, which is where we will copy our gash executable to when installing.
* --set-single-file-name; this sets the filename to write to for the generation stage - the assembled file is saved under `single-file-name`, and will overwrite any file of the same name. The default is `gash-local`.
* --set-install-name; this sets the name of our executable when we install with the --install flag.

The first time you run GASH, it will be compiled into guile vm code, which shouldnt take more than a minute (it takes ~ 5 seconds on a dual core celeron from 2009 at 1.6ghz)

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
2. Build in user commands and a way to call them. I'm thinking build a readline command that prompts for the name of a command and then runs it. similar to M-x in emacs.

### GASH as a main shell

It would be nice to have GASH able to function as a normal shell, instead of accessorizing the users default shell. This can be accomplished with system*, and then parsing for various control structures with a modified string-split which takes a string for what to split on. it could also be done where pipes are split using string-split, and everything else is divided up and organized after tokenization based on spaces. 

## Issues and Quirks

There are a couple quirks. Firstly, as GASH accessorizes the shell, and isnt itself a shell, there end up being some oddities with running it as your default shell. Since its not technically a shell, its not in /etc/shells, so you cant just set it as a default. Another way of launching it is to run `xterm -e /path/to/gash`.

Another quirk is strings. When you switch to scheme mode, and call a shell command (generally via ($ "...")) you need to make sure to escape strings. Take this example call: `$ echo '§(format #f "~a" ($ "ls §(format #f "/home")"))'` This call will error out, due to misquoted strings. Because the inner format contains a string while being contained within a string, the inner string needs to be escaped, like so: `$ echo '§(format #f "~a" (sh "ls §(format #f \"/home\")"))'`.

Another quirk is the `&&` and `||` logical operators. These MUST be preceded by an EXACT command and an EXACT command must follow. The text preceding and following these will be executed after forking. 

## The Name and History

This project was born from a simple desire - to be able to use format (and other scheme goodies) in the shell. The name was initially Guile Accessorizes the SHell, but I switched it to GASH Accessorizes the SHell. Either works though.

## (not so) Current Development notes

### Ideas for real shell functionality - &&, ||, and |

Were going to basically split the line up by what we want to do logically. eg pipes, and, or, and those types of operations. This means we will need to have a hierarchy of how we divide things.

Heres why:
take this line:
`do thing1 && do thing2 | thing3 || do thing 4`
We want to parse for `|`, `&&`, and `||`, of which all 3 occur. `&&` and `||` control process execution, and `|` feeds process' into each other. We must ask ourselves a question, which do we parse for first? Another way we can ask this is 'How do we want to interpret this line'. We want to turn this line into:
1. do thing1
2. if do thing1 exited success, go to 3, otherwise go to 5
3. pipe the output of running do thing2 into thing3
4. were done, be done and exit this thingy
5. do thing 4
6. were done, be done and exit this thingy
Because logical operators are different from redirection and pipes, we want to deal with our ands and ors first. lets deal with and (`&&`) first.

we end up with
`do thing1` `do thing2 | thing3 || do thing 4`
so we parse for or (`||`) and end up with
`do thing1` && `do thing2 | thing3` || `do thing 4`
and then we parse for pipe (`|`)
`do thing1` && `do thing2` | `thing3` || `do thing 4`
which we can convert into lispy psudocode:
(let ((thing1 (get-exit-status "do thing1")))
  (if thing1
      (let ((thing2 (pipe-x-into-y "do thing2" "thing3")))
        (if thing2
	    #t
	    (get-exit-status "do thing 4")))))

IDEA: we just need to get everything together like this after splitting it:
'("command one and args" "&&" "command2 arg | command3 arg" "||" "command4")
then we walk through it one by one, to generate a lambda or a series of lets. so we need to write it as a function. 

Take the following line:
`do thing1 || do thing2 | thing3 && do thing 4`
same as above, we want to turn this line into:
1. do thing1
2. if do thing1 exited success, exit and were done, otherwise continue
2a. do thing1 failed, so we want to
3. pipe do thing2 into thing3
4. if 3 returned success, do thing 4, otherwise,
5. exit were done.

lets run through this, starting with and (`&&`):
`do thing1 || do thing2 | thing3` && `do thing 4`
then parse for or (`||`)
`do thing1` || `do thing2 | thing3` && `do thing 4`


### Ideas for real shell functionality - redirection

This is pretty easy. just writing to files. 