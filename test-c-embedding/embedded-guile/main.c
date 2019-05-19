#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h> 
#include <readline/readline.h> 
#include <readline/history.h>
#include <libguile.h>
  
#define MAXCOM 1000 // max number of letters to be supported 
#define MAXLIST 100 // max number of commands to be supported 

// Clearing the shell using escape sequences 
#define clear() printf("\033[H\033[J")

int shell_fork_exec(char **args)
{
  pid_t pid, wpid;
  int status;

  pid = fork();
  if (pid == 0) {
    // were in the child
    if (execvp(args[0], args) == -1) {
      perror("gash child");
    }
    exit(EXIT_FAILURE);
  } else if (pid < 0) {
    // the fork failed...
    perror("gash fork fail");
  } else {
    // were in the parent
    do {
      wpid = waitpid(pid, &status, WUNTRACED);
    } while (!WIFEXITED(status) && !WIFSIGNALED(status));
  }
  return 1; 
}

SCM shell_fork_exec_scm_wrap(SCM list_of_args)
{
  int len = scm_to_int(scm_length(list_of_args));
  char *args[len + 1];
  int i;
  for (i = 0; i < len ; i++){
    args[i] = scm_to_locale_string
      (scm_list_ref(scm_list_tail(list_of_args, scm_from_int(i)),
		    scm_from_int(0)));
  }
  args[len] = NULL;
  return scm_from_int(shell_fork_exec(args));
}

void perr_scm_wrap(SCM text)
{
  perror(scm_to_locale_string(text));
}

SCM exit_in_humiliation_or_exhileration(SCM t_f)
{
  if (scm_is_false(t_f))
    exit(EXIT_FAILURE);
  else
    exit(EXIT_SUCCESS);
}

void load_scm_files()
{
  scm_c_primitive_load("script.scm");// load a script
  scm_c_primitive_load("gnasher.scm");
  scm_c_primitive_load("fork-and-exec.scm");
}

int main(int argc, char *argv[])
{
  /* 
     ok, so we want to initialize guile, so we will be in a single thread (i 
     guess), and forking will be safe. I think. I have no idea... 
  */
  SCM func_sym, func; // make two scheme variables

  scm_init_guile(); // initialize guile. 
  
  load_scm_files(); // load all our scheme files

  scm_c_define_gsubr
    ("fork-and-execute-a-list", 1, 0, 0, shell_fork_exec_scm_wrap);
  scm_c_define_gsubr ("exit-pass-fail", 1, 0, 0,
		      exit_in_humiliation_or_exhileration);
  scm_c_define_gsubr ("test-fork-exec", 0, 0, 0, test_fork_exec);
  scm_c_define_gsubr ("perror", 1, 0, 0, perr_scm_wrap);
  scm_c_define_gsubr ("load-all-gash-files", 0, 0, 0, load_scm_files);
  
  // get a variable, and look up the function it holds, and store that function
  func_sym = scm_c_lookup("main-entry");
  func = scm_variable_ref(func_sym);
  // call the function
  scm_call_0(func);
  exit(EXIT_SUCCESS);
}
