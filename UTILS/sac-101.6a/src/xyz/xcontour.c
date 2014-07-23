
#include <math.h>

#include "xyz.h"
#include "gam.h"
#include "gem.h"
#include "amf.h"
#include "bool.h"


#include "contouring.h"
#include "gtm.h"
#include "pl.h"
#include "bot.h"
#include "dfm.h"
#include "msg.h"
#include "gdm.h"
#include "cpf.h"
#include "co.h"
#include "gam.h"
#include "dff.h"

void /*FUNCTION*/ xcontour(nerr)
int *nerr;
{
	char kret[9];
	int lany, lframesave, lwait, lxlimits, lylimits;
	int ixstart, ixstop, iystart, iystop, jfile, ncret, 
	 ndxz, nfiles, nlen, notused, nxsize, nysize;
	float vportratio, vspaceratio, xmaximum, xminimum, xstart, xstop, 
	 ymaximum, yminimum, ystart, ystop;

	static char kwait[9] = "Waiting$";



	/*=====================================================================
	 * PURPOSE:  To execute the action command CONTOUR.
	 *           This command contours xyz data in memory.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error flag. Set to 0 if no error occurred. [i]
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:  xyz/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gem:     lframe, xpmn, xpmx, ypmn, ypmx
	 *    gam:     kgddef, lwaitr, lwaite
	 *    mem:     sacmem
	 *    xyz:     lzllist, zllist, nzllist, lzlmin, zlmin, lzlmax, zlmax,
	 *             lzlines, nzlines, izlines, nzregions, zregions
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gem:     lframe, ximn, ximx, yimn, yimx, ximnz, ximxz, yimnz, yimxz, 
	 *             xpmn, xpmx, ypmn, ypmx, xpmnu, xpmxu, ypmnu, ypmxu
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     vflist, vfxyz, getstatus, begindevice, getfil, getxlm,
	 *             indexb, beginframe, SetContDataMode, PlotContData, 
	 *             dispid, plhome, endframe, zgpmsg, upcase
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    kwait:   Message sent to terminal when in wait mode.
	 *    kret:    Message received from terminal when in wait mode.
	 *    nlcz:    Location in SACMEM array of each file's z data.
	 *    nlen:    Length of each data file.
	 *=====================================================================
	 * MODIFICATION HISTORY:
         *    970130:  Added arguments to dispid() not to plot file number. maf
	 *    901205:  Added text font selection from GTEXT command. 
	 *    900409:  Added coding so that XLIM and YLIM now set data limits.
	 *    900305:  Original version based upon XP.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900305
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- ASPECT ON|OFF:  maintain aspect ratio of data or not. */
		if( lklog( "ASPECT$",8, &cmxyz.laspect ) ){

			/* -- Bad syntax. */
			}
		else{
			cfmt( "ILLEGAL OPTION:",17 );
			cresp();

			}
		goto L_1000;
		}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

	if( *nerr != 0 )
		goto L_8888;

	/* CHECKING PHASE: */

	/* - Check for null data file list. */

	vflist( nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Check to make sure all files are xyz data files. */

	vfxyz( nerr );
	if( *nerr != 0 ){
		aplmsg( "Must be XYZ data files to use this command.",44 );
		goto L_8888;
		}

	/* - If no graphics device is open, try to open the default device. */

	getstatus( "ANY", &lany );
	if( !lany ){
		begindevice( kmgam.kgddef,9, nerr );
		if( *nerr != 0 )
			goto L_8888;
		}

	/* EXECUTION PHASE: */

	/* - Save current values of several GEM parameters. */

	lframesave = cmgem.lframe;
	plsave();

	/* - Check WAIT option.  This is on when:
	 * -- A wait request has been made.
	 * -- An active device (normally the user's terminal) is on. */

	if( cmgam.lwaitr ){
		getstatus( "ACTIVE", &lwait );
		}
	else{
		lwait = FALSE;
		}

	/* - For each file in DFL: */

	getnfiles( &nfiles );
	for( jfile = 1; jfile <= nfiles; jfile++ ){

		/* -- Get file from memory manager. */
		getfil( jfile, TRUE, &nlen, &ndxz, &notused, nerr );
		if( *nerr != 0 )
			goto L_8888;

		/* -- Get needed header information. */
		getnhv( "NXSIZE", &nxsize, nerr , 6 );
		if( *nerr != 0 )
			goto L_8888;
		getnhv( "NYSIZE", &nysize, nerr , 6 );
		if( *nerr != 0 )
			goto L_8888;
		getfhv( "XMINIMUM", &xminimum, nerr , 8);
		if( *nerr != 0 )
			goto L_8888;
		getfhv( "XMAXIMUM", &xmaximum, nerr , 8);
		if( *nerr != 0 )
			goto L_8888;
		getfhv( "YMINIMUM", &yminimum, nerr , 8);
		if( *nerr != 0 )
			goto L_8888;
		getfhv( "YMAXIMUM", &ymaximum, nerr , 8);
		if( *nerr != 0 )
			goto L_8888;

		/* -- Get requested x and y data limits and convert to data indices. */
		getxlm( &lxlimits, &xstart, &xstop );
		if( lxlimits ){
			cmgem.xgen.delta = (xmaximum - xminimum)/(float)( nxsize - 1 );
			ixstart = max( 1, (int)( (xstart - xminimum)/cmgem.xgen.delta ) + 1 );
			ixstop = min( nxsize, (int)( (xstop - xminimum)/cmgem.xgen.delta ) + 2 );
			xstart = xminimum + cmgem.xgen.delta*(float)( ixstart - 1 );
			xstop = xminimum + cmgem.xgen.delta*(float)( ixstop - 1 );
			}
		else{
			ixstart = 1;
			ixstop = nxsize;
			xstart = xminimum;
			xstop = xmaximum;
			}
		getylm( &lylimits, &ystart, &ystop );
		if( lylimits ){
			cmgem.ygen.delta = (ymaximum - yminimum)/(float)( nysize - 
			 1 );
			iystart = max( 1, (int)( (ystart - yminimum)/cmgem.ygen.delta ) + 
			 1 );
			iystop = min( nysize, (int)( (ystop - yminimum)/cmgem.ygen.delta ) + 
			 2 );
			ystart = yminimum + cmgem.ygen.delta*(float)( iystart - 1 );
			ystop = yminimum + cmgem.ygen.delta*(float)( iystop - 1 );
			}
		else{
			iystart = 1;
			iystop = nysize;
			ystart = yminimum;
			ystop = ymaximum;
			}
		setcontdatalim( ixstart, ixstop, iystart, iystop );

		/* -- Set PL/GEM world coordinates limits. */
		cmgem.ximn = xstart;
		cmgem.ximx = xstop;
		cmgem.yimn = ystart;
		cmgem.yimx = ystop;
		cmgem.zdata.xmin = xstart;
		cmgem.zdata.xmax = xstop;
		cmgem.zdata.ymin = ystart;
		cmgem.zdata.ymax = ystop;

		/* -- Begin next plot frame if requested. */
		if( lframesave ){
			beginframe( FALSE , nerr );
			if( *nerr != 0 )
				goto L_8888;
			}

		/* -- Rectify old PL/GEM viewport values and current graphics library ones. */
		getratio( &vspaceratio );
		cmgem.uplot.xmin = cmgem.plot.xmin;
		cmgem.uplot.xmax = cmgem.plot.xmax;
		cmgem.uplot.ymin = cmgem.plot.ymin * vspaceratio;
		cmgem.uplot.ymax = cmgem.plot.ymax * vspaceratio;
		setvport( cmgem.uplot.xmin, cmgem.uplot.xmax, cmgem.uplot.ymin, cmgem.uplot.ymax );

		/* -- Set viewport border and viewport ratio. */
		if( cmxyz.laspect ){
			vportratio = fabs( (ystop - ystart)/(xstop - xstart) );
			setvportratio( vportratio );
			getvport( &cmgem.uplot.xmin, &cmgem.uplot.xmax, &cmgem.uplot.ymin, &cmgem.uplot.ymax );
			}

		/* -- Set the text font from gtext parameter */
		settextfont( cmgem.igtfnt );

		/* -- Contour the data. */
		plotcontdata( cmmem.sacmem[ndxz], nxsize, nysize, nerr );
		if( *nerr != 0 )
			goto L_8888;

		/* -- Plot the axes, labels andframe id; home cursor. */
		plcalwvtrans();
		plgrid( nerr );
		if( *nerr != 0 )
			goto L_8888;
		dispid( 0 , 0, 0, NULL );
		plhome();
		flushbuffer( nerr );
		if( *nerr != 0 )
			goto L_8888;

		/* -- End current plot frame if requested. */
		if( lframesave )
                  endframe( FALSE , nerr );
                else 
                  flushbuffer( nerr );

		/* -- Wait for user prompt before plotting next frame if appropriate. */
		if( jfile == nfiles && !cmgam.lwaite )
			lwait = FALSE;
		if( lwait ){
			zgpmsg( kwait,9, kret,9 );
			ncret = indexb( kret,9 );
			upcase( kret, ncret, kret,9 );
			if( kret[0] == 'K' )
				goto L_8888;
			if( kret[0] == 'G' )
				lwait = FALSE;
			}
		}

	/* - Restore GEM parameters before returning. */

L_8888:
	cmgem.lframe = lframesave;
	plrest();
	return;

} /* end of function */

