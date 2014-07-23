/** 
 * @file   zbasename.c
 * 
 * @brief  Get the SAC base directory
 * 
 */
#include <stdlib.h>
#include <string.h>

#include "co.h"

char *
sacaux() {
  static char *aux = NULL;
  if(aux) {
    return aux;
  }
  if((aux = getenv("SACAUX"))) {
    return aux;
  }
  fprintf(stderr, "ERROR: Environmental variable SACAUX not defined.\n");
  exit(1);
  return NULL;
}

/** 
 * Get the name of the SAC base directory, will exit if the SACAUX variable
 *    is not defined.  String is padded with spaces and then null-terminated.
 * 
 * @param name 
 *    Expanded base directory on output
 * @param name_len 
 *    Length of \p name
 *
 * @date  04/19/87      Original version based upon zexpnd.
 * @date  04/22/87      Modified to blank fill returned base name.
 * @date  02/02/88      Modified to exit if SACAUX is not defined.
 *
 */
void
zbasename(char *name,
	  int   name_len) {

  char *aux;
  aux = sacaux();
  if((int)strlen(aux) > name_len-1) {
    fprintf(stderr, "ERROR: Enviornment variable SACAUX too long: max: %d SACAUX: %d\n",
            name_len-1, (int)strlen(aux));
    exit(1);
  }
  memset(name, ' ', name_len);
  name[name_len-1] = '\0';

  strncpy(name, aux, strlen(aux));
  name[name_len-1] = '\0';

  return;
}

            
