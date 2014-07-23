#include <string.h>

#include "extfunc.h"

void fgetfhdr_(hdr_index, fieldname, value, error, lenfield)
int *hdr_index;
char *fieldname;
float    *value;
int     *error;
int   lenfield;
{
  char infield[8];
  sac_header *hdr_in;

  hdr_in = indata->ext_hdrs[*hdr_index-1];

  memset(infield,' ',8);
  infield[8]='\0';

  getfield(fieldname, lenfield, infield);

  *value = getfhdr(hdr_in, infield, error);

  return;

}
