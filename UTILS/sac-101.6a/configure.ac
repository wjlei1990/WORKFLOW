

# Process this file with autoconf to produce a configure script.
AC_INIT(Seismic Analysis Code, 101.6a, sac-help@iris.washington.edu, sac)

# Place configuration files in the config directory
AC_CONFIG_AUX_DIR(config)

# Default install location
AC_PREFIX_DEFAULT(/usr/local/sac)

# Do not re-run autotools 
# Do not recreate the configure script if the time stamps are not correct
AM_MAINTAINER_MODE

# Determine the System Type
AC_CANONICAL_SYSTEM

# Header Values 
AC_CONFIG_HEADER(inc/config.h)

# Initialization of Automake
AM_INIT_AUTOMAKE 

m4_include([config/matlab.m4])
m4_include([config/png.m4])
m4_include([config/xpm.m4])
m4_include([config/zlib.m4])
m4_include([config/xft.m4])

# Check for CFLAGS Variable and set default to ""
AS_IF( [ test x"${CFLAGS}" = x ],  [ CFLAGS="" ] )

# Check for LDFLAGS Variable and set default to ""
AS_IF( [ test x"${LDFLAGS}" = x ], [ LDFLAGS="" ] )

AC_SUBST([BINARY_DIST])
BINARY_DIST=""

# Readline/Editline Handling (Editline On by Default, except for Cygwin)
editline=off
readline=off
winaux=off

# Operating System Checks and Sets
case "${host}" in
     i[[3456]]86-*-linux-gnu*)       AC_DEFINE( [LINUX],   [1], [ Linux/x86 Operating System ])   
                                     BINARY_DIST="linux_x86"
                                     MATLAB_BIN_PATH="bin/glnx86"
                                     MATLAB_EXE_PATH="bin/matlab"
                                     CFLAGS+=" -D_GNU_SOURCE "
     ;;
     x86_64-*-linux-gnu*)            AC_DEFINE( [LINUX],   [1], [ Linux/x86/64 Operating System ])   
                                     BINARY_DIST="linux_x86_64"
                                     MATLAB_BIN_PATH="bin/glnxa64"
                                     MATLAB_EXE_PATH="bin/matlab"
                                     CFLAGS+=" -D_GNU_SOURCE "
     ;;
     sparc*-sun-solaris2.[[56789]]*) AC_DEFINE( [SOLARIS], [1], [ SUN/Solaris Operating System ]) 
                                     BINARY_DIST="sun_solaris2"
                                     MATLAB_BIN_PATH="bin/sol64"
                                     MATLAB_EXE_PATH="bin/matlab"
     ;;
     sparc*-sun-solaris*)            AC_DEFINE( [SOLARIS], [1], [ SUN/Solaris Operating System ]) 
                                     BINARY_DIST="sun_solaris"
                                     MATLAB_BIN_PATH="bin/sol64" 
                                     MATLAB_EXE_PATH="bin/matlab"    
     ;;
     *darwin[[789]]* | *darwin[[912]]* ) AC_DEFINE( [OSX],     [1], [ Apple/OSX/Darwin Operating System ]) 
                                     BINARY_DIST="OSX_darwin"    
                                     MATLAB_EXE_PATH="bin/matlab" 
                                     case "${host}" in
                                          *i686*)    MATLAB_BIN_PATH="bin/maci"  ;;
                                          *powerpc*) MATLAB_BIN_PATH="bin/mac"   ;;
                                          *x86_64*)  MATLAB_BIN_PATH="bin/mac"   ;;
                                     esac
     ;;
     *darwin6* )                     AC_DEFINE( [OSX],     [1], [ Apple/OSX/Darwin Operating System ])
                                     BINARY_DIST="OSX_darwin6"     
                                     LDFLAGS="-L/sw/lib $LDFLAGS"
                                     CFLAGS="-I/sw/include $CFLAGS"
                                     MATLAB_EXE_PATH="bin/matlab"
                                     case "${host}" in
                                          *i686*)    MATLAB_BIN_PATH="bin/maci"  ;;
                                          *powerpc*) MATLAB_BIN_PATH="bin/mac"   ;;
                                     esac
     ;;
     *cygwin* )                      AC_DEFINE( [LINUX],   [1], [ Unix Emulation ] )
                                     AC_DEFINE( [CYGWIN],  [1], [ Cygwin32 System ] )
                                     BINARY_DIST="win32"     
                                     readline=on
                                     MATLAB_BIN_PATH="bin/win32"
                                     MATLAB_EXE_PATH="bin/matlab"
				     AC_MSG_CHECKING([for Cygwin version])
                                     OSRELEASE=`uname -r`
				     case "${OSRELEASE}" in 
                                          *1.[[135]]*) winaux=on
                   			  ;;
                                     esac
				     AC_MSG_RESULT( $OSRELEASE )
     ;;
esac


AC_SUBST([CURSES_SEARCH_PATH])
curses_library_found=no
AC_DEFUN([NCURSES_ON], [ CURSES_SEARCH_PATH="ncurses curses termcap" ])
AC_DEFUN([NCURSES_OFF], [ CURSES_SEARCH_PATH="curses termcap" ])
AC_ARG_ENABLE(ncurses, AS_HELP_STRING([--disable-ncurses],[avoid the ncurses library]),
                       [ AS_IF( [ test x$enableval != xyes ], [ NCURSES_OFF ], [ NCURSES_ON ]) ],
                       [ NCURSES_ON ]
)

# Macro Definitions
AC_DEFUN([READLINE_ON],[ AC_DEFINE([READLINE], [1], [Compile with command line capabilities]) ])
AC_DEFUN([CURSES], [
    for termlib in ${CURSES_SEARCH_PATH} ; do
        AC_CHECK_LIB(${termlib}, tgoto, [LIBS="-l${termlib} $LIBS"; curses_library_found=yes; break], [ test ])
    done
    if test "$curses_library_found" = no ; then
        AC_MSG_ERROR([Developer Library ncurses, curses, or termcap not found, please install them])
    fi
    ]
)
AC_DEFUN([EDITLINE_TURN_OFF], [ AC_DEFINE([EDITLINE_DISABLED], [1], [Compile without editline comand line capabilities]) ])
AC_DEFUN([EDITLINE_TURN_ON],  [ editline=on ] )
AC_DEFUN([READLINE_TURN_ON],  [ readline=on ] )

# Readline (OFF by Default)
AC_ARG_ENABLE(readline, AS_HELP_STRING([--enable-readline],[enable readline command line capabilities]), 
    [ AS_IF( [ test x$enableval == xyes ], [ READLINE_TURN_ON ] ) ],
    [ test ] 
)

# Editline (ON by Default)
AC_ARG_ENABLE(editline, AS_HELP_STRING([--disable-editline],[disable editline command line capabilities]), 
    [ AS_IF( [ test x$enableval == xno ], [ EDITLINE_TURN_OFF ], [ EDITLINE_TURN_ON ] )],
    [ AS_IF( [ test x$readline  != xon ], [ EDITLINE_TURN_ON ] )]
)
AM_CONDITIONAL([EDITLINE], [ test x$editline = xon ])
AM_CONDITIONAL([READLINE], [ test x$readline = xon ])

# Handle the Editline Library
AS_IF( [ test x$editline = xon ], [
   CURSES
   READLINE_ON
   editline=on
   readline=off
] )

# Handle the Readline Library
AS_IF( [ test x$readline = xon ], [
   CURSES
   READLINE_ON
   AC_CHECK_HEADERS([ readline/readline.h ],
        AC_SEARCH_LIBS([add_history], [readline]),
    [AC_MSG_FAILURE( --enable-readline was given, but test for readline failed)] )
   EDITLINE_TURN_OFF
   editline=off
   readline=on   
] )

m4_include([config/xdr.m4])

# 
AM_CONDITIONAL([CYGWIN_SYSTEM], [ test x${BINARY_DIST} = xwin32 ])
AM_CONDITIONAL([WINAUX_NOT_AUX], [ test x${winaux} = on ] )

# X11 Double Buffering 
x11_buffer=off
AC_DEFUN([X11_BUFFER_ON], 
         [ AC_DEFINE([USE_X11_DOUBLE_BUFFER], 
                 [1], 
             [Compile with X11 double buffers]) 
             x11_buffer=on ])
AC_DEFUN([X11_BUFFER_OFF], 
         [ AC_DEFINE([USE_X11_DOUBLE_BUFFER_DISABLED], 
                 [1], 
             [Compile without X11 double buffers]) 
             x11_buffer=off ])
AC_ARG_ENABLE(x11_buffer, AS_HELP_STRING([--disable-x11-buffer],[disable X11 double buffer]),   
      [ AS_IF( [  test x$enableval != xyes ], [ X11_BUFFER_OFF ], [ X11_BUFFER_ON ] ) ],
      [ X11_BUFFER_ON ] 
)

# X11 Multiple Color Depths
x11_depth=off
AC_DEFUN([X11_DEPTH_ON], 
         [ AC_DEFINE([USE_X11_MULTIPLE_DEPTHS], 
                     [1], 
                     [Compile with X11 multiple color depths]) 
             x11_depth=on ] )
AC_DEFUN([X11_DEPTH_OFF], 
         [ AC_DEFINE([USE_X11_MULTIPLE_DEPTHS_DISABLED], 
                     [1], 
                     [Compile without X11 multiple color depth])
             x11_depth=off ] )
AC_ARG_ENABLE(x11_depth, AS_HELP_STRING([--disable-x11-depth],[disable X11 multiple color depth]),  
      [ AS_IF( [ test x$enableval != xyes ], [ X11_DEPTH_OFF ], [ X11_DEPTH_ON ] ) ],
      [ X11_DEPTH_ON ] 
)

# Check for Matlab Options
CHECK_MATLAB

# SeisMgr Database Access
database=off
AC_DEFUN([DATABASE_ON], 
     [ AC_DEFINE([DATABASE_ENABLED],  
                 [1], 
                 [Compile with SeisMgr Database Capabilities]) 
                 database=on ])
AC_DEFUN([DATABASE_OFF], 
     [ AC_DEFINE([DATABASE_DISABLED], 
                 [1], 
                 [Compile without SeisMgr Database Capabilities]) 
                 database=off ])
AC_ARG_ENABLE(database, AS_HELP_STRING([--enable-database],[enable SeisMgr database capabilities]), 
       [ AS_IF( [ test x$enableval != xno ], [ DATABASE_ON ], [ DATABASE_OFF ] ) ], 
       [ DATABASE_OFF ] 
)
AM_CONDITIONAL([DATABASE], [ test x$database = xon ])

# Debug
AC_SUBST([DEBUG])
AC_DEFUN([DEBUG_OFF], [ DEBUG="" ])
AC_ARG_ENABLE(debug, AS_HELP_STRING([--enable-debug],[enable debugging options]), 
        [ AS_IF( [ test x$enableval != xno ], [  DEBUG="-g -Wall -Wextra" ], [  DEBUG_OFF ] ) ], 
        [ DEBUG_OFF ] 
)

# Coverage
AC_SUBST([COVERAGE])
AC_DEFUN([COVERAGE_OFF], [ COVERAGE="" ])
AC_ARG_ENABLE(coverage, AS_HELP_STRING([--enable-coverage],[enable coverage builds]), 
        [ AS_IF( [ test x$enableval != xno ], [  COVERAGE="--coverage" ], [  COVERAGE_OFF ] ) ], 
        [ COVERAGE_OFF ] 
)


# Optimization
AC_SUBST([OPTIM])
AC_DEFUN([OPTIM_OFF], [ OPTIM="" ])
AC_ARG_ENABLE(optim, AS_HELP_STRING([--enable-optim=level],[enable optimization level]),
    [ if test x$enableval != xno; then
         OPTIM=-O${enableval}
      else
         OPTIM_OFF
      fi
    ], [ OPTIM_OFF ] 
)


# C Compiler
AC_PROG_CC

# Linking Files
AC_PROG_LN_S

# Sed - Stream Editor, Handles SAC_PREFIX for bin/sacinit.[sh,csh]
AC_PROG_SED

# Source and Objects in the Same Directory
AM_PROG_CC_C_O

AC_PROG_F77([f77 g77 gfortran xlf xlf90])

AM_CONDITIONAL([HAVE_F77], [ test x"$ac_cv_prog_f77_g" != x"" ] )

# X11 Path and Libaries
no_x=
AC_PATH_XTRA

if test "$no_x" == yes ; then
   AC_MSG_ERROR([Cannot find X11 Developer Libraries and Include files, please install them])
fi

CHECK_XPM
CHECK_ZLIB
CHECK_PNG
CHECK_XFT

# Checks for programs.
AC_PROG_RANLIB

# Check for header files
#AC_PATH_X
#AC_HEADER_STDC
#AC_CHECK_HEADERS([string.h strings.h termio.h termios.h sgtty.h sys/termios.h])

AC_CHECK_HEADERS([ inttypes.h limits.h stdint.h ])

# Check for Terminal Window Size Location
AC_HEADER_TIOCGWINSZ

# Math Libary
AC_CHECK_LIB(m, sin)

# Dynamic Loading of Modules
AC_CHECK_LIB(dl, dlopen)

# Check for Specific Functions 
AC_CHECK_FUNC(lround,    [ AC_DEFINE( [HAVE_FUNC_LROUND],        [1], [ System Libraries define lround ]) ],
                         [ AC_DEFINE( [MISSING_FUNC_LROUND],     [1], [ System Libraries missing lround ]) ]  )
AC_CHECK_FUNC(round,     [ AC_DEFINE( [HAVE_FUNC_ROUND],         [1], [ System Libraries define round ]) ],
                         [ AC_DEFINE( [MISSING_FUNC_ROUND],      [1], [ System Libraries missing round ]) ]  )
AC_CHECK_FUNC(copysign,  [ AC_DEFINE( [HAVE_FUNC_COPYSIGN],      [1], [ System Libraries define copysign ]) ],
                         [ AC_DEFINE( [MISSING_FUNC_COPYSIGN],   [1], [ System Libraries missing copysign ]) ]  )
AC_CHECK_FUNC(fmin,      [ AC_DEFINE( [HAVE_FUNC_FMIN],          [1], [ System Libraries define fmin ]) ],
                         [ AC_DEFINE( [MISSING_FUNC_FMIN],       [1], [ System Libraries missing fmin ]) ]  )
AC_CHECK_FUNC(fmax,      [ AC_DEFINE( [HAVE_FUNC_FMAX],          [1], [ System Libraries define fmax ]) ],
                         [ AC_DEFINE( [MISSING_FUNC_FMAX],       [1], [ System Libraries missing fmax ]) ]  )
AC_CHECK_FUNC(tolower,   [ AC_DEFINE( [HAVE_FUNC_TOLOWER],       [1], [ System Libraries define tolower ]) ],
                         [ AC_DEFINE( [MISSING_FUNC_TOLOWER],    [1], [ System Libraries missing tolower ]) ]  )
AC_CHECK_FUNC(toupper,   [ AC_DEFINE( [HAVE_FUNC_TOUPPER],       [1], [ System Libraries define toupper ]) ],
                         [ AC_DEFINE( [MISSING_FUNC_TOUPPER],    [1], [ System Libraries missing toupper ]) ]  )
AC_CHECK_FUNC(strtof,    [ AC_DEFINE( [HAVE_FUNC_STRTOF],        [1], [ System Libraries define strtof ]) ] )
AC_DEFUN([SETENV_ON],    [ AC_DEFINE( [HAVE_FUNC_SETENV],        [1], [ System Libraries define setenv ]) ])
AC_DEFUN([PUTENV_ON],    [ AC_DEFINE( [HAVE_FUNC_PUTENV], [1], [ System Libraries define putenv ]) ])
AC_DEFUN([MISSING_SETPUT], [ AC_MSG_FAILURE( [ System does have a working setenv or putenv function ] ) ] )
AC_CHECK_FUNC(setenv, [ SETENV_ON ], [ AC_CHECK_FUNC(putenv,  [ PUTENV_ON ], [ MISSING_SETPUT ]) ] )
AC_CHECK_FUNC(vasprintf, [ AC_DEFINE( [HAVE_FUNC_VASPRINTF],     [1], [ System Libraries define vasprintf ]) ],
                         [ AC_DEFINE( [MISSING_FUNC_VASPRINTF],  [1], [ System Libraries missing vasprintf ]) ]  )
AC_CHECK_FUNC(strsep,    [ AC_DEFINE( [HAVE_FUNC_STRSEP],        [1], [ System Libraries define strsep ]) ],
                         [ AC_DEFINE( [MISSING_FUNC_STRSEP],     [1], [ System Libraries missing strsep ]) ]  )
AC_CHECK_FUNC(mkstemps,  [ AC_DEFINE( [HAVE_FUNC_MKSTEMPS],      [1], [ System Libraries define mkstemps ]) ],
                         [ AC_DEFINE( [MISSING_FUNC_MKSTEMPS],   [1], [ System Libraries missing mkstemps ]) ]  )

# Check for typedefs, structures, and compiler characteristics
#AC_C_CONST

# Define the Build Date 
AC_DEFINE_UNQUOTED([BUILD_DATE], ["`date +%m/%d/%Y`"], [Macro which defines the Date of the Current Build])

# Output the Makefiles
AC_OUTPUT(src/Makefile utils/Makefile libedit/Makefile t/Makefile Makefile)
AC_MSG_NOTICE([

Compilation Options for $PACKAGE_TARNAME ($PACKAGE_STRING)

CFLAGS         = $CFLAGS 
LDFLAGS        = $LDFLAGS
LIBS           = $LIBS 
Debug          = $DEBUG $COVERAGE
Optimization   = $OPTIM
Database       = $database
Matlab         = $have_matlab
X11-buffer     = $x11_buffer
X11-depth      = $x11_depth
XDR Format     = $librpc
Support        = $xpm$png$zlib$xft$freetype$fontconfig
X_LIBS         = $X_LIBS 
X_CFLAGS       = $X_CFLAGS
Readline       = $readline
Editline       = $editline
Install Base   = $prefix
System         = $BINARY_DIST ($host)

To compile $PACKAGE_TARNAME type make ])
