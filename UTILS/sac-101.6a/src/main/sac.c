/** 
 * @file   sac.c
 * 
 * @brief  Main execution loop, start here
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "mach.h"
#include "exm.h"
#include "bool.h"
#include "select.h"
#include "history.h"

#include "config.h"


#include "bot.h"
#include "ucf.h"
#include "msg.h"
#include "top.h"
#include "bbs.h"
#include "co.h"
#include "ncpf.h"
#include "ssi.h"

void set_constrain_plot_ratio_x11( int set );

/** 
 * Main command execution loop for SAC, called by the system on startup
 * 
 * @param argc 
 *    Number of command line arguments
 * @param argv 
 *    Command line arguments
 * 
 * @return 
 *    - 0 on Success
 *    - Non-Zero on Failure
 *
 * @note Variables
 *   - kmsg:    Message received from terminal. [c]
 *   - ncmsg:   Length of KMSG without trailing blanks. [i]
 *   - kline:   Message after being processed to substitute blackboard
 *              and header variables. [c]
 *   - ncline:  Length of KLINE without trailing blanks. [i]
 *   - nerr:    Error return flag. [i]
 *             =0 if ok.
 *             >0 if message was invalid or command encountered errors.
 *
 * @date   900804:  Minor change in subroutine names.
 * @date   891005:  General cleanup: included call to zstart for system
 *                  dependent startup code; deleted some obsolete code;
 *                  changed calling logic to saccommands.
 * @date   870421:  Added call to processline.
 * @date   870109:  Changed to zgpmsg to handle "graphics events."
 * @date   841219:  Retyped on a ROS3.2/RIDGE 32, removed interrupt control.
 * @date   820923:  Added interrupt control and documented subroutine.
 * @date   820430:  Mod due to change in ZGPMSG.
 * @date   810000:  Original version.
 *
 */
int
main(int    argc, 
     char **argv ) {

	char kline[MCMSG+1], kmsg[MCMSG+1];
	int ic, ic1, ic2, itype, nc, ncmsg, nerr, i;
  char *macro;
  int macro_start;
  
	/* initialize kmsg.  maf 970630 */
        memset(&(kmsg[0]), ' ', MCMSG);
	memset(&(kline[0]), ' ', MCMSG);
	kmsg[0] = '\0' ;
	kmsg[MCMSG] = '\0' ;
	kline[MCMSG] = '\0' ;
  macro_start = 0;
  
	/* - Initialize common. */
    for( i=1; i<argc; i++ ){
      if(strcmp(argv[i], "--copyright-off") == 0) {
        display_copyright( OPTION_OFF );
        macro_start ++;
      }
      else if(strcmp(argv[i], "--copyright-on") == 0) {
        display_copyright( OPTION_ON );
        macro_start ++;
      }
    }
	initsac();

    for( i=1; i<argc; i++ ){
      if(strcmp(argv[i], "--bell-off") == 0) {
        bell_off();
        macro_start ++;
      }
      else if(strcmp(argv[i], "--bell-on") == 0) {
        bell_on();
        macro_start ++;
      }
      else if(strcmp(argv[i], "--letter") == 0) {
        set_constrain_plot_ratio_x11( TRUE );
        macro_start ++;
      }
      if(strcmp(argv[i], "--no-tty") == 0) {
        tty_force(OPTION_OFF);
        macro_start ++;
      }
      if(strcmp(argv[i], "--stdout") == 0) {
        sac_output_stdout();
        sac_warning_stdout();
        sac_error_stdout();
        macro_start ++;
      }
      if(strcmp(argv[i], "--show-prompt") == 0) {
        show_prompt_without_tty(OPTION_ON);
        macro_start ++;
      }
      if(strcmp(argv[i], "--database-on") == 0) {
        use_database(OPTION_ON);
        macro_start ++;
      }
      if(strcmp(argv[i], "--database-off") == 0) {
        use_database(OPTION_OFF);
        macro_start ++;
      }
      if(strcmp(argv[i], "--history-off") == 0) {
        use_history(OPTION_OFF);
        macro_start ++;
      }
      if(strcmp(argv[i], "--history-on") == 0) {
        use_history(OPTION_ON);
        macro_start ++;
      }
      if(strcmp(argv[i], "--set-default-station-name") == 0) {
        set_default_station_name(OPTION_ON);
        macro_start ++;
      }
      if(strcmp(argv[i], "--gdb-debug") == 0) {
        tty_force(OPTION_OFF);
        sac_output_stdout();
        sac_warning_stdout();
        sac_error_stdout();
        macro_start ++;
        show_prompt_without_tty(OPTION_ON);
      }
    }
    
	/* - Get the input line message, if any.
	 *   This should be the name of the a default SAC macro to execute. */

	zgimsg(argc-macro_start,argv + macro_start,kmsg,MCMSG+1 );
	nc = indexb( kmsg,MCMSG+1 );
	if( nc > 0 ){
	    ic = 0;
	    poptok( kmsg, nc, &ic, &ic1, &ic2, &itype );
	    setmsg( "COMMAND", 99 );
	    apcmsg( "(INPUT LINE) MACRO",19 );
	    apcmsg( kmsg,MCMSG+1 );
	    outmsg();
	    clrmsg();

      asprintf(&macro, "macro %s", kmsg);
      saccommands(macro, strlen(macro), &nerr);
      if(macro) {
        free(macro);
        macro = NULL;
      }
	}

	/* - THIS IS THE MAIN LOOP OF THE PROGRAM.
	 *   (1) "zgpmsg" sends a prompt to the user and gets a message back.
	 *   (2) The message is first passed through "processline" which evaluates any
	 *       blackboard or header variables and substitutes them into the message.
	 *   (3) The message is then passed to "saccommands" which parses and executes
	 *       the command(s) that are contained within the message.
	 *   (4) When all the commands have been executed or an error has
	 *       occured, "saccommands" returns and the process is repeated.
	 *   (5) Program termination is handled by the QUIT command found
	 *       in the Executive Module (subroutine "xexmc"). */

	/* the main loop of the program */
	while( TRUE ){
    zgpmsg( kmexm.kprmt,13, kmsg,MCMSG+1 );
    if(! AddToHistory(kmsg) ) {
      continue;
    }
    
    ncmsg = indexb( kmsg,MCMSG+1 );
    if( ncmsg >= MCMSG ){
      setmsg( "ERROR", 99 );
      apcmsg("Error: Cmd line exceeds buffer limited to num of chars: "
             ,57 );
      apimsg( MCMSG );
      outmsg();
      clrmsg();
      continue ;
    }

    /* Remove prompt at beginning of a line */
    if(strncasecmp(kmsg, "SAC> ", 5) == 0) {
      memmove(&kmsg[0], &kmsg[5], MCMSG-5);
    }

    setmsg( "COMMAND", 99 );
    apcmsg( kmsg,MCMSG+1 );	/* Add message to the list in kmmsg */
    outmsg();		/* Write message to appropriate output devices */
    clrmsg();		/* Clear message from list in kmmsg */

    /* saccommands:  executes the commands in kline. */
    saccommands( kmsg, MCMSG+1, &nerr );
  }
}

