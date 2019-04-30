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
  buffer = readline(printer);
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

SCM get_string_from_user_scm_wrapper()
{
  char str[MAXCOM];
  int x = get_string_from_user(str);
  if (x == 0)
    return scm_from_locale_string(str);
  // return scm_from_locale_string(str);
  return scm_from_locale_string("");
}

// int get_string_from_user_with_prompt(char* str, char* prompt)
SCM get_string_from_user_with_prompt_scm_wrapper(SCM prompt)
{
  char str[MAXCOM];
  int x = get_string_from_user_with_prompt(str, scm_to_locale_string(prompt));
  return scm_from_locale_string(str);
}

void init_gash_c()
{
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
