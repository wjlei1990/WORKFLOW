/** 
 * @file   history.c
 * 
 * @brief  History command
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "history.h"
#include "select.h"
#include "bool.h"
#include "exm.h"

void
history_print() {
  int j;
  HIST_ENTRY *he;

  while( next_history() ){ 
    continue;
  }
  j = -1;

  for(he = current_history(); he != NULL; he = previous_history()) {
    if (j > 0) {
      printf("%5d  %s\n",  j, he->line);
    }
    j++;
  }

  return;
}


#define SAC_USE_HISTORY "SAC_USE_HISTORY"

#define HISTORY_COMMAND "history"
#define HISTORY_COMMAND_LENGTH strlen( HISTORY_COMMAND )

#define READLINE_COMMAND_EXPANSION  1
#define READLINE_COMMAND_PRINT      2

/* ************** Exported functions ************** */

int
use_history(int getset) {
  static int virgin = TRUE;
  static int using_history = TRUE;
  if(getset == OPTION_ON || getset == OPTION_OFF) {
    using_history = getset;
  }
  if(virgin) {
    virgin = FALSE;
    using_history = env_bool(SAC_USE_HISTORY, using_history);
  }
  return using_history;
}


int
AddToHistory(char *line) {

  int result;
  char *expansion;
  char *p;

  result = history_expand(line, &expansion) ;

  if(result == READLINE_COMMAND_PRINT) {
    printf("%s\n", expansion);
    free(expansion);
    line[0] = ' ';
    line[1] = 0;
    return FALSE;
  }
  
  if(result < 0) {
    free(expansion);
    line[0] = ' ';
    line[1] = 0;
    return FALSE;
  }

  if(result == READLINE_COMMAND_EXPANSION) {
    strncpy(line, expansion, strlen(expansion));
    line[strlen(expansion)] = 0;
  }

  if(strncmp(line, HISTORY_COMMAND, HISTORY_COMMAND_LENGTH) == 0) {
    history_print();
  }
  free(expansion);
  
  /* Check for empty lines */
  p = &line[0];
  while(isspace(p[0])) { p++; };
  if(p[0] == '\0') {
    line[0] = 0;
  }
  if(strlen(line) > 0 && use_history( OPTION_GET ) ) {
    add_history(line);
  }

  return TRUE;

}
