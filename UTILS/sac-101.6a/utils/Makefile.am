
# Do not create dependencies while compiling
AUTOMAKE_OPTIONS = no-dependencies

AM_CFLAGS           = $(DEBUG) $(COVERAGE) $(OPTIM)

# Compiled programs to install
bin_PROGRAMS = sgftops sgfswap saclst sacswap bbfswap unvis
sgftops_SOURCES = sgftops.c
sgfswap_SOURCES = sgfswap.c
saclst_SOURCES = saclst.c sac.h
sacswap_SOURCES = sacswap.c 
bbfswap_SOURCES = bbfswap.c 
unvis_SOURCES = unvis.c

bbfswap_LDADD = ../src/libsacio.a 

EXTRA_DIST = Makefile.utils \
	readsac.m writesac.m \
	padcat.m getsacdata.m \
	README_utils





