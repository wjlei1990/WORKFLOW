
#include "xyz.h"
#include "gdm.h"


#include "gd2.h"
#include "gd3.x11.h"

void 
cbar_window(unsigned int xloc,
            unsigned int yloc,
            unsigned int height,
            unsigned int w_height,
            unsigned int w_width,
            float vspaceratio,
            float ypmax,
            int *nerr)
{

    *nerr = 0;

    if( Lgdon[2] )
        cbar_window2(xloc,yloc,height,w_height,w_width,vspaceratio,ypmax,nerr);

    if( Lgdon[3] )
        cbar_window3(xloc,yloc,height,w_height,w_width,vspaceratio,ypmax,nerr);

    return;
} /* end of function */







