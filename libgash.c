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

char* cat3(const char *s1, const char *s2, const char *s3)
{
  char *ret = malloc(strlen(s1) + strlen(s2) + strlen(s3) + 1);
  strcpy(ret, s1);
  strcat(ret, s2);
  strcat(ret, s3);
  return ret;
}

char* concatenate(const char *s1, const char *s2, const char *s3, const char *s4)
{
  char *ret = malloc(strlen(s1) + strlen(s2) + strlen(s3) + strlen(s4) + 1);
  strcpy(ret, s1);
  strcat(ret, s2);
  strcat(ret, s3);
  strcat(ret, s4);
  return ret;
}

// get stuff from the user.
int get_string_from_user(char* str)
{
  char* buffer;
  char* prompt;
  char* pre_prompt;
  char *user=getenv("USER");
  char hostname [1024];
  gethostname(hostname, 1023);
  char cwd[PATH_MAX];
  if (getcwd(cwd, sizeof(cwd) - 1) == NULL){
    strcpy(cwd, "ERR finding directory");
  }
  // we need to purge all but the last entity of cwd
  
  
  pre_prompt = concatenate("[\033[38;5;6m", user, "@", hostname);
  prompt = concatenate(pre_prompt, " ", cwd, "\033[0m]» ");
  free(pre_prompt);
  // char* username = getenv("USER");
  buffer = readline(prompt);
  free(prompt);
  /* if (buffer && *buffer){ */
  /*   add_history(buffer); */
  /*   strcpy(str, buffer); */
  /*   return 0; */
  /* } */
  if (strlen(buffer) != 0) {
    add_history(buffer);
    strcpy(str, buffer);
    return 0;
  } else {
    return 1;
  }
}

int get_string_from_user_with_prompt(char* str, char* prompt)
{
  char* buffer;
  char* directory;
  // char* username = getenv("USER");
  // readline uses \001 and \002 to determine what should be counted when
  // counting the prompt characters. 
  char* printer = cat3("[\001\033[38;5;6m\002", prompt, "\001\033[m\002]» ");
  // dont forget to free printer - its cat-ed so we need to free it. 
  fflush(stdout);
  // printf("in read-with-prompt, before reading\n");
  buffer = readline(printer);
  // printf("after reading\n");
  // buffer = readline ("prompt"); 
    
  free(printer);
  if (buffer == NULL){
    return 1;}
  if (strlen(buffer) != 0) {
    add_history(buffer);
    strcpy(str, buffer);
    return 0;
  } else {
    return 1;
  }
}

/*
char **command_generator(const char *text, int state)
{
  SCM function_sym = scm_c_lookup("command-completer");
  SCM func_val = scm_variable_ref(function_sym);
  SCM ret_val = scm_call_2(func_val, scm_from_locale_string(text),
			   scm_from_bool(state));
  if (scm_is_bool(ret_val) == 1)
    return ((char *)NULL);
  return scm_from_locale_string(ret_val);
}

char **smart_completion(char *text, int start, int end)
{
  char **matches;

  matches = (char **)NULL;

  if (start == 0)
    matches = rl_completion_matches(text, command_generator);
  return (matches); 
}

char **completer_test(const char *text, int state)
{
  SCM function_sym = scm_c_lookup("command-completer");
  SCM func_val = scm_variable_ref(function_sym);
  SCM ret_val = scm_call_2(func_val, scm_from_locale_string(text),
			   scm_from_bool(state));
  if (scm_is_bool(ret_val) == 1)
    return ((char *)NULL);
  return scm_from_locale_string(ret_val);
}

void initialize_readline ()
{
  rl_readline_name = "GASH";
  rl_attempted_completion_function = smart_completion;
}
*/


SCM get_string_from_user_scm_wrapper()
{
  char str[MAXCOM];
  int x = get_string_from_user(str);
  if (x == 0)
    return scm_from_locale_string(str);
  // return scm_from_locale_string(str);
  return scm_from_locale_string("");
}

SCM is_string_directory(SCM path)
{
  struct stat sb;
  int exists = stat(scm_to_locale_string(path), &sb);
  if (exists == -1)
    return scm_from_bool(0);
  if (exists == 0 && S_ISDIR(sb.st_mode))
    return scm_from_bool(1);
  return scm_from_bool(0);
}

// int get_string_from_user_with_prompt(char* str, char* prompt)
SCM get_string_from_user_with_prompt_scm_wrapper(SCM prompt)
{
  char str[MAXCOM];
  int x = get_string_from_user_with_prompt(str, scm_to_locale_string(prompt));
  if (x == 0)
    return scm_from_locale_string(str);
  return scm_from_bool(0);
}

SCM scheme_strncmp(SCM string1, SCM string2, SCM length)
{
  return scm_from_int
    (strncmp(scm_to_locale_string(string1),
	     scm_to_locale_string(string2),
	     scm_to_int(length))); 
}

SCM scheme_expose_filename_completion_function(SCM text, SCM state)
{
  // printf("within exposed function\n");
  char *str = rl_filename_completion_function(scm_to_locale_string(text),
					      scm_to_int(state));
  // printf("after filename completion called\n");
  if (str == NULL){
    // printf("str is NULL\n");
    return scm_from_bool(0);
  }
  // printf("str is not NULL\n");
  return scm_from_locale_string(str);
}

SCM scm_readline_generator_function_var;

char * scheme_generator_function(char *text, int state)
{
  char *ret;
  SCM res, t, s;
  // printf("in scheme_generator_function\n");
  SCM strf = SCM_VARIABLE_REF(scm_readline_generator_function_var);
  // printf("pre if in scheme_generator_function\n");
  if (scm_is_false(strf)){
    printf("in the if in scheme_generator_function\n");
    return ((char *) NULL);
  }
  // printf("post if in scheme_generator_function\n");
  t = scm_from_locale_string(text);
  s = scm_from_bool(state);
  // printf("pre res set\n");
  res = scm_apply (strf, scm_list_2(t, s), SCM_EOL);
  // printf("res is set\n");
  if (scm_is_false(res)){
    return ((char *)NULL);
  }
  ret = scm_to_locale_string(res);
  // printf("weve assigned ret in scheme_generator_function\n");
  
  return ret;
}

SCM scm_get_rl_line_buffer()
{
  return scm_from_locale_string(rl_line_buffer);
}

SCM scm_readline_alt_completion_function_var;
SCM scm_readline_matches_fun;

char **
alt_completer(char *text, int start, int end)
{
  SCM compf = SCM_VARIABLE_REF (scm_readline_alt_completion_function_var);
  SCM res, t, s, e;
  int length, i;
  char ** reter = (char **)NULL;
  // printf("pre if statement\n");
  if (scm_is_false (compf)){
    // printf("completer-for-alt returned false"); 
    return reter;
  }
  else {
    // printf("compf WASNT false\n");
    t = scm_from_locale_string(text);
    s = scm_from_int(start);
    e = scm_from_int(end);
    // printf("pre scm_apply\n"); 
    res = scm_apply (compf, scm_list_3 (t, s, e), SCM_EOL);
    // printf("passed res = scm_apply (compf...)\n"); 
    if (scm_is_false (res)) {
      // printf("res is false\n"); 
      return reter;
    }
    // printf("about to set reter...\n");
    reter = rl_completion_matches(text, scheme_generator_function);
    // printf("reter is set\n"); 
    /* for (i = 0; i < length ; i++) {
      reter[i] = scm_to_locale_string(scm_car(res));
      printf(reter[i]);
      printf("\n");
      res = scm_cdr(res);
    } */
    
    return reter ; //(reter); 
  }
}

/* int log(char *text) */
/* { */
/*   FILE *log; */
/*   log = fopen("gash.log", "a"); */
/*   if (log == NULL){ */
/*     fclose(log); */
/*     return 0; */
/*   } */
/*   fprintf(log, text); */
/*   fclose(log); */
/*   return 1; */
/* } */

/* SCM scm_write_to_log(SCM text) */
/* { */
/*   int ret = log(scm_to_locale_string(text)); */
/*   return scm_from_bool(ret); */
/* } */

void init_gash_c()
{
  /* FILE * logg = fopen("gash.log", "w"); */
  /* fclose(logg); */
  /* int i = log("beginning GASH log\n\n"); */
  
  scm_readline_generator_function_var
    = scm_c_define("*readline-generator-function*", SCM_BOOL_F);
  scm_readline_alt_completion_function_var
    = scm_c_define("*readline-alt-completion-function*", SCM_BOOL_F);
  rl_attempted_completion_function = (rl_compentry_func_t*) alt_completer;

  // scm_c_define_gsubr("write-to-log", 1, 0, 0, scm_write_to_log);
  scm_c_define_gsubr("get-line-contents", 0, 0, 0, scm_get_rl_line_buffer);
  scm_c_define_gsubr("exposed-file-completion-function", 2, 0, 0,
		     scheme_expose_filename_completion_function);
  scm_c_define_gsubr
    ("c-strncmp", 3, 0, 0, scheme_strncmp);
  scm_c_define_gsubr
    ("get-string-from-user", 0, 0, 0, get_string_from_user_scm_wrapper);
  scm_c_define_gsubr
    ("read-with-prompt", 1, 0, 0, get_string_from_user_with_prompt_scm_wrapper);
  scm_c_define_gsubr
    ("directory?", 1, 0, 0, is_string_directory);
}

/* compile with:
gcc `pkg-config --cflags guile-2.2` -shared -o lib-c-funcs.so -fPIC c-funcs.c -l readline
 */
