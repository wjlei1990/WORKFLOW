
# Do not create dependencies while compiling
# Place object files in the same directory as the sources

# Currently all libraries and the sac binary are found in
# the src directory, not the lib or bin directory
# Using the make install option will move everything
# to the appropriate directories specified by --prefix 
# during configure

AUTOMAKE_OPTIONS  = subdir-objects no-dependencies


FORMAT_CFLAGS        = $(xpm_CFLAGS) $(png_CFLAGS) $(zlib_CFLAGS)
# Compilation Flags
AM_CFLAGS           = $(DEBUG) $(COVERAGE) $(OPTIM) $(xft_CFLAGS) $(FORMAT_CFLAGS) $(X_CFLAGS) -I$(top_srcdir)/inc

# Compiled Programs 
bin_PROGRAMS      = sac
sac_SOURCES       = main/sac.c

# Libraries to be installed
lib_LIBRARIES     = libsacio.a libsac.a

# Libraries not installed
noinst_LIBRARIES  = libsac_all.a \
		    libevalresp.a \
		    libSMnonOracleFuncs.a \
	            libsac_check.a \
	            libsac_x11.a \
		    libsgf.a libturtle.a 


# Libraries needed to compile sac
#  libsac_all.a is included twice to satisfy recursive dependencies; fix
sac_LDADD         = -L$(srcdir) libsac_all.a libsgf.a libsac_x11.a \
	                libturtle.a \
	                libsac_all.a \
			libevalresp.a \
			libSMnonOracleFuncs.a

# SeisMgr Database Capabilities
if DATABASE
noinst_LIBRARIES += libSMOracleFuncs.a
sac_LDADD        += libSMOracleFuncs.a
else
noinst_LIBRARIES += libSacOracleStubFuncs.a 
sac_LDADD        += libSacOracleStubFuncs.a
endif

FORMAT_LIBS      = $(xpm_LIBS) $(png_LIBS) $(zlib_LIBS) 
# Libraries needed to compile sac (continued ...)
sac_LDADD        += $(X_PRE_LIBS) $(FORMAT_LIBS) $(xft_LIBS) $(X_EXTRA_LIBS) $(X_LIBS) -lX11


# Readline/Editline Dependencies (Default = ON)
if EDITLINE
sac_LDADD        += ../libedit/lib/libedit.a
AM_CFLAGS           += -I../libedit/include
endif


EXTRA_DIST       = describedirs gd3.x11/sac.bitmap \
		   history/History.doc \
		   SeisMgr/dbselect/dbHelpQuery.pc \
		   SeisMgr/dbselect/dbQuery.pc \
		   SeisMgr/dbselect/dbQuerySL.pc \
		   dfm/xra.c evalresp/evalresp.c gam/test.c \
		   icm/oracle/dbFuncs.pc \
	           icm/oracle/dbaseCommand.c icm/thisfft.c mat/stationList.c \
		   ncpf/hrtoe.c ncpf/readableToEpoch.c \
		   ssi/xrecall.c

.SILENT: mostlyclean-compile

# Compile Rules for the libsacio
bbs/readbbf_io.o: $(srcdir)/bbs/readbbf.c
	$(COMPILE) -DDOINITS -c $(srcdir)/bbs/readbbf.c -o $@
dff/hdrfld_io.o: $(srcdir)/dff/hdrfld.c
	$(COMPILE) -DDOINITS -c $(srcdir)/dff/hdrfld.c -o $@
msg/outmsg_io.o: $(srcdir)/msg/outmsg.c
	$(COMPILE) -DDOINITS -c $(srcdir)/msg/outmsg.c -o $@

evalresp/evresp.o: $(srcdir)/evalresp/evresp.c
	$(COMPILE) -DLIB_MODE -DLOG_LABEL -c $(srcdir)/evalresp/evresp.c -o $@
evalresp/parse_fctns.o: $(srcdir)/evalresp/parse_fctns.c
	$(COMPILE) -DLIB_MODE -c $(srcdir)/evalresp/parse_fctns.c -o $@
evalresp/string_fctns.o: $(srcdir)/evalresp/string_fctns.c
	$(COMPILE) -DLIB_MODE -c $(srcdir)/evalresp/string_fctns.c -o $@

CLEANFILES        = bbs/readbbf_io.o dff/hdrfld_io.o msg/outmsg_io.o

libsacio_a_LIBADD = bbs/readbbf_io.o dff/hdrfld_io.o msg/outmsg_io.o

libsacio_a_SOURCES = amf/allamb.c amf/iniam.c amf/relamb.c \
	bbs/getbbv.c bbs/inibbs.c bbs/setbbv.c bbs/unsetbbv.c \
	bbs/writebbf.c \
	\
	bot/crname.c bot/indexa.c bot/indexb.c \
	bot/modcase.c bot/nequal.c \
	\
	co/fstrncpy.c co/math.c \
	co/subscpy.c co/zbasename.c co/zclose.c co/zclosec.c \
	co/zcloses.c co/zdest.c co/zgetc.c co/zinquire.c \
	co/znfile.c co/zopen.c co/zopenc.c \
	co/zopens.c co/zputc.c co/zrabs.c co/zsysop.c \
	co/zwabs.c \
	\
	dff/getfhv.c dff/getihv.c dff/getkhv.c \
	dff/getlhv.c dff/getnhv.c dff/inihdr.c dff/inilhf.c \
	dff/map_chdr_in.c dff/map_chdr_out.c dff/newhdr.c \
	dff/rsac1.c dff/rsac2.c dff/setfhv.c dff/setihv.c \
	dff/setkhv.c dff/setlhv.c dff/setnhv.c  dff/wsac0.c \
	dff/wsac1.c dff/wsac2.c dff/f2c.c \
	dff/wsac3.c dff/rsach.c \
	\
	msg/apcmsg.c msg/apcmsg2.c \
	msg/apcmsgnum.c msg/apimsg.c msg/aplmsg.c msg/clrmsg.c \
	msg/getsmsg.c msg/inimsg.c msg/sacmsg.c msg/sendmesg.c \
	msg/setmsg.c msg/typmsg.c msg/wrtmsg.c msg/apfmsg.c \
	\
	ucf/byteswap.c \
	ucf/cnvita.c ucf/fill.c ucf/ljust.c  \
	ucf/extrma.c ucf/distaz.c \
	\
  vars/createvlist.c \
	vars/deletev.c vars/deletevlist.c \
	vars/getvFILEptr.c \
	vars/getvvstring.c \
	vars/initializevar.c \
	vars/inivars.c vars/putvFILEptr.c \
	vars/putvvstring.c vars/readvfile.c \
	vars/writevfile.c \
	vars/var.c vars/chash.c vars/bbf.c vars/chash.h \
	\
	string/strings.c


libSMnonOracleFuncs_a_SOURCES = \
	SeisMgr/smDataIO.c \
	SeisMgr/smWorkSets.c \
	SeisMgr/stringfun.c \
	SeisMgr/cssb/cssRead2_8FlatFiles.c \
	SeisMgr/cssb/cssReadFlatFiles.c \
	SeisMgr/cssb/cssWriteFlatFiles.c \
	SeisMgr/cssb/cssbIO.c \
	SeisMgr/cssListOps/cssListOps.c \
	SeisMgr/cssListOps/cssTableDump.c \
	SeisMgr/cssListOps/dbFillWfdisc.c \
	SeisMgr/cssListOps/dblErrors.c \
	SeisMgr/cssListOps/dblGetDataSubs.c \
	SeisMgr/cssListOps/dblUserData.c \
	SeisMgr/cssListOps/enlarge.c \
	SeisMgr/cssListOps/g2tofloat.c \
	SeisMgr/cssListOps/isValid.c \
	SeisMgr/gc/gcChecks.c \
	SeisMgr/gc/gcCollect.c \
	SeisMgr/gse/GSEWaveforms.c \
	SeisMgr/gse/cmprs6.c \
	SeisMgr/gse/dcomp.c \
	SeisMgr/gse/gseRead20.c \
	SeisMgr/gse/gseWrite20.c \
	SeisMgr/sacIO/dbConversions.c \
	SeisMgr/sacIO/sacInput.c \
	SeisMgr/sacIO/sacOutput.c \
	SeisMgr/smMemory/smMemory.c \
	SeisMgr/suds/sudsDataConversions.c \
	SeisMgr/suds/sudsIO.c \
	SeisMgr/suds/sudsListOps.c \
	SeisMgr/suds/sudsReading.c \
	SeisMgr/suds/sudsToCSSlist.c \
	SeisMgr/suds/sudsWriting.c \
	SeisMgr/time/timefuncs.c 

libSacOracleStubFuncs_a_SOURCES = SeisMgr/smNoDBstubs.c \
	icm/nonOracle/dbaseCommand.c

libSMOracleFuncs_a_SOURCES = SeisMgr/smDBdata.c \
	SeisMgr/dbselect/dbselect.c \
	SeisMgr/dbselect/dbBuildSQLstring.c \
	SeisMgr/dbselect/dbPtsInside.c \
	SeisMgr/dbselect/dbDefaults.c \
	SeisMgr/dbselect/dbBuildSearchlink.c \
	SeisMgr/dbselect/dbHelp.c 
# These are .pc files 
#	SeisMgs/dbselect/dbQuery.c \
#	SeisMgs/dbselect/dbQuerySL.c \
#	SeisMgr/dbselect/dbQueryHelp.c

libevalresp_a_SOURCES = evalresp/alloc_fctns.c evalresp/calc_fctns.c \
	evalresp/error_fctns.c evalresp/evr_spline.c evalresp/evresp.c \
	evalresp/file_ops.c evalresp/parse_fctns.c evalresp/print_fctns.c \
	evalresp/regerror.c evalresp/regexp.c evalresp/regsub.c \
	evalresp/resp_fctns.c evalresp/string_fctns.c \
	evalresp/evr_spline.h evalresp/evresp.h evalresp/regexp.h \
	evalresp/regmagic.h

libsac_a_SOURCES = co/complex.c  \
	\
	dfm/cut.c \
	\
	dbh/xapiir.c dbh/apply.c dbh/design.c \
	dbh/lp.c dbh/lptbp.c dbh/lptbr.c dbh/lpthp.c dbh/warp.c \
	dbh/cutoffs.c dbh/chebparm.c dbh/buroots.c dbh/beroots.c \
	dbh/c1roots.c dbh/c2roots.c dbh/bilin2.c \
	dbh/firtrn.c dbh/overlp.c \
	dbh/zero.c dbh/zshft.c dbh/fft.c dbh/crscor.c \
	dbh/window.c dbh/rms.c \
	\
	icm/trans.c icm/dcpft.c icm/taper.c icm/getran.c \
	\
  ucf/next2.c ucf/copydouble.c \
	\
	sam/envelope.c \
	\
	scm/rmean.c scm/rtrend.c scm/taper.c scm/interpolate.c scm/wigint.c scm/rotate.c


libsac_all_a_SOURCES = amf/allamb.c amf/iniam.c amf/reaamb.c amf/relamb.c \
	\
	bbs/createbbs.c bbs/deletebbs.c bbs/getbbv.c \
	bbs/inibbs.c bbs/readbbf.c bbs/setbbv.c bbs/unsetbbv.c bbs/writebbf.c \
	\
	bom/getbfl.c bom/inibom.c bom/relbfl.c bom/xaddf.c \
	bom/xboec.c bom/xbomc.c bom/xdivf.c bom/xmerge.c bom/xmulf.c \
	bom/xsubf.c \
	\
	bot/capf.c bot/chpf.c bot/clipdp.c bot/crname.c \
	bot/indexa.c bot/indexb.c bot/indexc.c bot/lequal.c bot/locdp.c \
	bot/modcase.c bot/nequal.c bot/terminate.c bot/upcase.c bot/wrlist.c \
	\
	clf/fndelcl.c clf/ldelcl.c clf/lnumcl.c clf/lnxtcl.c clf/nfndcl.c \
	clf/putcl.c \
	\
	cnd/getclun.c cnd/getdolen.c cnd/ldolist.c cnd/skipdo.c \
	cnd/skipif.c cnd/xbreak.c cnd/xcndc.c cnd/xdo.c cnd/xelse.c \
	cnd/xelseif.c cnd/xenddo.c cnd/xendif.c cnd/xif.c cnd/xwhile.c \
	\
	co/backspace.c co/complex.c co/ext_init.c co/fstrncpy.c co/getfline.c \
	co/getline.c co/izshft.c co/math.c co/select.c \
	co/strscpy.c co/subscpy.c co/tokenize.c co/zauxfile.c co/zbasename.c \
	co/zclose.c co/zclosec.c co/zcloses.c co/zdest.c co/zdestf.c \
	co/zexecute.c co/zfiles.c co/zgetc.c co/zgetgd.c co/zgimsg.c \
	co/zgpmsg.c co/zgtmsg.c co/zgwindowsize.c co/zinquire.c \
	co/zload.c co/znfile.c co/znfiles.c co/zopen.c \
	co/zopenc.c co/zopens.c co/zputc.c co/zquit.c co/zrabs.c co/zrun.c \
	co/zrunname.c co/zruntext.c co/zsleep.c co/zsysop.c co/zwabs.c \
	co/stdu.h \
	\
	coda/binary_op.c coda/calc_codaGF.c coda/calc_coda_amplitudes.c \
	coda/calc_energy.c coda/calc_envelopes.c coda/calc_moment_magnitude.c \
	coda/fit_coda_amp.c coda/fit_coda_params.c coda/get_input.c \
	coda/pickwindows.c coda/plotspec.c \
	coda/send_output.c coda/unary_op.c coda/xcoda.c \
	\
	contouring/alloclabels.c contouring/allocpoints.c \
	contouring/allocsegments.c contouring/calccontlab1.c \
	contouring/calccontlab4.c contouring/calccontlabs.c \
	contouring/calccontrlink.c contouring/calccontsegs.c \
	contouring/calccontticks.c contouring/calclastpoint.c \
	contouring/calcsegangle.c contouring/fastcontdata.c \
	contouring/getcontlabel.c contouring/getcontpoint.c \
	contouring/getcontrlink.c contouring/getcontseg.c \
	contouring/getcontseglab.c contouring/initcontattr.c \
	contouring/linkcontsegs.c contouring/listcontsegs.c \
	contouring/markcontlabel.c contouring/mergecontsegs.c \
	contouring/newcontlabel.c contouring/newcontpoint.c \
	contouring/newcontseg.c contouring/nextcontseg.c \
	contouring/plotcontdata.c contouring/plotcontsegs.c \
	contouring/pointsequal.c contouring/putcontlabel.c \
	contouring/putcontpoint.c contouring/putcontrlink.c \
	contouring/putcontseg.c contouring/putcontseglab.c \
	contouring/releaselabels.c contouring/releasepoints.c \
	contouring/releasesegmen.c contouring/setcontdatali.c \
	\
	cpf/cerr.c cpf/cfmt.c cpf/cresp.c \
	cpf/inicom.c cpf/initcomlists.c \
	cpf/lcchar.c cpf/lccl.c cpf/lcdfl.c \
	cpf/lcia.c cpf/lcidi.c cpf/lcint.c cpf/lcirc.c cpf/lcircp.c \
	cpf/lckey.c cpf/lckeyExact.c cpf/lclist.c cpf/lclog.c cpf/lclog2.c \
	cpf/lcmore.c cpf/lcquot.c cpf/lcra.c cpf/lcreal.c cpf/lcrest.c \
	cpf/lcrrcp.c cpf/lcrtw.c cpf/lkchar.c cpf/lkcharExact.c \
	cpf/lkentries.c cpf/lkia.c cpf/lkint.c cpf/lkirc.c cpf/lklist.c \
	cpf/lklog.c cpf/lklog2.c cpf/lklogc.c cpf/lklogi.c cpf/lklogr.c \
	cpf/lklogra.c cpf/lkquot.c cpf/lkra.c cpf/lkreal.c cpf/lkrest.c \
	cpf/lkrrc.c cpf/lkrrcp.c cpf/lkrtw.c cpf/lkt.c cpf/non_num_com.c \
  cpf/setcomlist.c \
	cpf/xclog.c cpf/xclogr.c cpf/xcrrcp.c cpf/xcrtw.c \
	\
	datafilelist/nextinputfile.c datafilelist/selectinputfi.c \
	datafilelist/setinputmode.c \
	\
	dbh/afr.c dbh/alias.c dbh/append.c \
	dbh/apply.c dbh/autcor.c dbh/beroots.c dbh/bilin2.c dbh/buroots.c \
	dbh/c1roots.c dbh/c2roots.c dbh/chebparm.c dbh/cpolar.c dbh/crit.c \
	dbh/crscor.c dbh/cutoffs.c dbh/decim.c dbh/design.c dbh/dfr.c \
	dbh/dircor.c dbh/edecim.c dbh/fft.c dbh/firtrn.c dbh/gauss.c \
	dbh/getResponse.c dbh/iirfilter.c dbh/inspect.c dbh/levin.c \
	dbh/levinD.c dbh/linpack.c dbh/lp.c dbh/lptbp.c dbh/lptbr.c \
	dbh/lpthp.c dbh/mem.c dbh/mlm.c dbh/overlp.c dbh/pds.c dbh/pef.c \
	dbh/phaseshift.c dbh/pltplr.c dbh/random.c dbh/rms.c dbh/shift.c \
	dbh/spectr.c dbh/step.c dbh/unit.c dbh/warp.c dbh/wiener.c \
	dbh/window.c dbh/winmov.c dbh/xapiir.c dbh/zero.c dbh/zshft.c \
	dbh/ACKNOWLEDGEMENT \
	\
	dff/formhv.c dff/formmarker.c dff/getfhv.c dff/getfil.c \
	dff/gethv.c dff/getihv.c dff/getkhv.c dff/getlhv.c dff/getnfiles.c \
	dff/getnhv.c dff/hdrfld.c dff/inihdr.c dff/inilhf.c dff/lgahdr.c \
	dff/map_chdr_in.c dff/map_chdr_out.c dff/map_hdr_in.c \
	dff/map_hdr_out.c dff/markhdr.c dff/newhdr.c \
	dff/putfil.c dff/rddta.c dff/rdhdr.c dff/rdsac.c dff/rdsdta.c \
	dff/rdsegy.c dff/rdshdr.c dff/rdxdrdta.c dff/rsac1.c dff/rsac2.c \
	dff/setfhv.c dff/setihv.c dff/setkhv.c dff/setlhv.c dff/setnfiles.c \
	dff/setnhv.c dff/setrng.c dff/updhdr.c dff/wrsac.c dff/wrsdd.c \
	dff/wrsegy.c dff/wrxdr.c dff/wsac0.c dff/wsac1.c dff/wsac2.c \
	dff/xdrhdr.c dff/f2c.c dff/wsac3.c dff/rsach.c \
	\
	dfm/cleardfl.c dfm/cnvfmt.c dfm/cnvfre.c dfm/crsac.c \
	dfm/decont.c dfm/defcut.c dfm/defmem.c dfm/detnum.c dfm/gennames.c \
	dfm/getatw.c dfm/getprefs.c dfm/getwfdiscs.c dfm/inidfm.c \
	dfm/iztypeMessage.c dfm/lfilesok.c dfm/makeuniq.c \
	dfm/rdci.c dfm/readcfl.c dfm/readfl.c dfm/readgse.c dfm/readsuds.c \
	dfm/sort.c dfm/synch.c dfm/updatedfl.c dfm/vblist.c dfm/vfeven.c \
	dfm/vflist.c dfm/vfmax.c dfm/vfmaxn.c dfm/vfrng.c dfm/vfspec.c \
	dfm/vftime.c dfm/wrci.c dfm/xch.c dfm/xconv.c \
	dfm/xcopyhdr.c dfm/xcuter.c dfm/xdatagen.c dfm/xdeletechannel.c \
	dfm/xdfmc.c dfm/xlh.c dfm/xpickauthor.c dfm/xpickphase.c \
	dfm/xpickprefs.c dfm/xr.c dfm/xrcss.c dfm/xrerr.c \
	dfm/xrgse.c dfm/xrh.c dfm/xrsdd.c dfm/xrsuds.c dfm/xrtab.c \
	dfm/xsort.c dfm/xsynch.c dfm/xw.c dfm/xwcss.c dfm/xwgse.c dfm/xwh.c \
	dfm/xwild.c dfm/xwtab.c dfm/cut.c \
	\
	eam/inieam.c eam/whpf1.c eam/xapk.c \
	eam/xeamc.c eam/xoapf.c eam/xohpf.c eam/xwhpf.c  \
	\
	exm/gettextwait.c \
	exm/iniexm.c exm/proerr.c exm/qam.c exm/qapf.c exm/qcolor.c \
	exm/qcut.c exm/qdevices.c exm/qfid.c exm/qgtext.c exm/qhpf.c \
	exm/qline.c exm/qmtw.c exm/qpicks.c exm/qsymbol.c exm/qtitle.c \
	exm/qwidth.c exm/qxlabl.c exm/qxlim.c exm/qylabl.c exm/qylim.c \
	exm/repav.c exm/reperr.c exm/repiv.c exm/repivl.c exm/repkv.c \
	exm/replv.c exm/reprtw.c exm/reprv.c exm/setprompt.c \
	exm/settextwait.c exm/tracereport.c exm/tracevariable.c exm/xabout.c \
	exm/xcd.c exm/xecho.c exm/xeval.c exm/xexmc.c exm/xfg.c exm/xgetbb.c \
	exm/xhelp.c exm/xinstallmacr.c exm/xload.c exm/xmacro.c exm/xmsg.c \
	exm/xnews.c exm/xpause.c exm/xreadbbf.c exm/xreport.c exm/xsetbb.c \
	exm/xsetmacro.c exm/xsyntx.c exm/xsystemcomma.c exm/xtrace.c \
	exm/xtranscript.c exm/xunsetbb.c exm/xwritebbf.c \
	\
	extfuncs/fextlib.c \
	extfuncs/fgetahdr.c extfuncs/fgetehdr.c extfuncs/fgetfhdr.c \
	extfuncs/fgetlhdr.c extfuncs/fgetnhdr.c extfuncs/fsetahdr.c \
	extfuncs/fsetehdr.c extfuncs/fsetfhdr.c extfuncs/fsetlhdr.c \
	extfuncs/fsetnhdr.c extfuncs/getahdr.c extfuncs/getehdr.c \
	extfuncs/getfhdr.c extfuncs/getfield.c extfuncs/getlhdr.c \
	extfuncs/getnhdr.c extfuncs/makehdr.c extfuncs/setahdr.c \
	extfuncs/setehdr.c extfuncs/setfhdr.c extfuncs/setlhdr.c \
	extfuncs/setnhdr.c \
	\
	fks/calcBeamOffsets.c fks/calcoffsets.c \
	fks/inifks.c fks/xbbfk.c fks/xbeam.c fks/xfksc.c fks/xgmtmap.c \
	fks/xmap.c \
	\
	gam/dispid.c gam/disppk.c gam/disppkLandscape.c \
	gam/getxlm.c gam/getylm.c gam/inigam.c gam/markvert.c gam/markwf.c \
	gam/pcmcur.c gam/pcmrpl.c gam/pcrrpl.c gam/pcxcur.c gam/pcxop1.c \
	gam/pcxop2.c gam/pcxope.c gam/pcxops.c gam/pcxrpl.c gam/pkchar.c \
	gam/pkdet.c gam/pkeval.c gam/pkfilt.c gam/pkfunc.c \
	gam/wavfrm.c gam/xfid.c gam/xfilenumber.c gam/xfitxy.c gam/xgamc.c \
	gam/xp.c gam/xp1.c gam/xp2.c gam/xpc.c gam/xpicks.c gam/xplotalpha.c \
	gam/xplotdy.c gam/xplotpm.c gam/xplotxy.c gam/xppk.c gam/xprint.c \
	gam/xsetdevice.c gam/xylim.c \
	\
	gcm/xbegindevice.c gcm/xenddevices.c \
	gcm/xgcmc.c gcm/xsgf.c gcm/xvspac.c \
	\
	gdm/adj_geometry.c \
	gdm/begindevice.c gdm/begindevices.c gdm/beginframe.c \
	gdm/beginwindow.c gdm/calstatus.c gdm/calvspace.c gdm/changectable.c \
	gdm/convcolornam.c gdm/convcolornum.c gdm/createwindow.c \
	gdm/cursor0.c gdm/cursoroff.c gdm/cursoron.c gdm/cursortext.c \
	gdm/draw.c gdm/enddevice.c gdm/endframe.c gdm/endgraphics.c \
	gdm/erase.c gdm/fill_colorbar.c gdm/fill_image.c gdm/flushbuffer.c \
	gdm/get_geometry.c gdm/getalphainfo.c gdm/getdevicein1.c \
	gdm/getdevicenam.c gdm/getdevicerat.c gdm/getlinestyle.c \
	gdm/getmaxdevice.c gdm/getratio.c gdm/getstatus.c gdm/getstringsiz.c \
	gdm/gettextjust.c gdm/gettextsize.c gdm/getvspace.c \
	gdm/getvspacetyp.c gdm/getwindowsta.c gdm/inigdm.c gdm/initctable.c \
	gdm/loadctable.c gdm/move.c gdm/put_image.c gdm/readctable.c \
	gdm/setcolor.c gdm/setcolorname.c gdm/setlinestyle.c \
	gdm/setlinewidth.c gdm/setpsctable.c gdm/settextangle.c \
	gdm/settextfont.c gdm/settextjust.c gdm/settextsize.c \
	gdm/settexttype.c gdm/setvspacecli.c gdm/setvspacetyp.c \
	gdm/softwaretext.c gdm/text.c \
	gdm/postscript.c gdm/record.c gdm/pdf.c \
	\
	gem/inisym.c gem/xaxes.c \
	gem/xbeginframe.c gem/xbeginwindow.c gem/xcolor.c gem/xendframe.c \
	gem/xgemc.c gem/xgrid.c gem/xgt.c gem/xlct.c gem/xline.c gem/xplab.c \
	gem/xqdp.c gem/xsym.c gem/xticks.c gem/xtitle.c gem/xtsize.c \
	gem/xwait.c gem/xwidth.c gem/xwindow.c gem/xxdiv.c gem/xxgrid.c \
	gem/xxlab.c gem/xydiv.c gem/xygrid.c gem/xylab.c gem/set_skel_fudge.c \
	\
	gpm/begingraphic.c \
	\
	gtm/calwvtransfo.c gtm/getvport.c gtm/getworld.c gtm/inigtm.c \
	gtm/line.c gtm/polyline.c gtm/rectangle.c gtm/setsymbolgap.c \
	gtm/setsymbolnum.c gtm/setsymbolsiz.c gtm/setvport.c \
	gtm/setvportrat.c gtm/setworld.c gtm/symbol.c gtm/vporttoworld.c \
	gtm/worldcursor.c gtm/worlddraw.c gtm/worldline.c gtm/worldmove.c \
	gtm/worldpolylin.c gtm/worldsector.c gtm/worldtovport.c gtm/xaxis.c \
	gtm/xlinaxis.c gtm/xlogaxis.c gtm/yaxis.c gtm/ylinaxis.c \
	gtm/ylogaxis.c \
	\
	history/history.c \
	\
	icm/EVRESPnames.c \
	icm/EvrespGateway.c icm/acc.c icm/balance.c icm/bbdisp.c icm/bbvel.c \
	icm/benbog.c icm/ckinst.c icm/clh.c icm/clz.c icm/cmh.c icm/cmz.c \
	icm/csh.c icm/csz.c icm/dcpft.c icm/dewit.c icm/dseis.c icm/dss.c \
	icm/dwwssn.c icm/ekalp6.c icm/ekasp2.c icm/elmag.c icm/eyeomg.c \
	icm/gbalp.c icm/gbasp.c icm/general.c icm/getins.c icm/getran.c \
	icm/getroots.c icm/gsref.c icm/hfslpwb.c icm/hqr.c icm/hs3.c \
	icm/iniicm.c icm/lll.c icm/llsn.c icm/lrsmlp.c icm/lrsmsp.c \
	icm/noress.c icm/noresshf.c icm/oldbb.c \
	icm/oldkir.c icm/polezero.c icm/portable.c \
	icm/predfl.c icm/prewit.c icm/ptbllp.c icm/redkir.c icm/reftek.c \
	icm/rs7.c icm/rsk.c icm/rsl.c icm/rsm.c icm/rstn.c icm/sandia.c \
	icm/snla3.c icm/sro.c icm/taper.c icm/transfer.c \
	icm/vel.c icm/wa.c icm/wabn.c icm/wiech.c icm/wwlpbn.c icm/wwsp.c \
	icm/wwspbn.c icm/xicmc.c icm/xprewit.c icm/xtransfer.c icm/ykalp.c \
	icm/ykasp.c icm/fap.c \
	icm/dbFuncs.h \
	icm/trans.c \
	\
	mat/engineCall.c mat/engineCall2.c mat/matBlackBoardVars.c \
	mat/matConnect.c mat/matDisconnect.c mat/matHeaderDataManage.c \
	mat/matStructArrayOps.c mat/stationSets.c mat/xCLOSEMAT.c mat/xMAT.c \
	mat/xMAT3C.c mat/xmatc.c mat/xsetmat.c \
	mat/matFuncExternal.h mat/matFuncInternal.h \
	mat/matHeaderDataManage.h mat/matStructArrayOps.h \
	mat/matStructDefs.h \
	\
	msg/apcmsg.c msg/apcmsg2.c \
	msg/apcmsgnum.c msg/apfmsg.c msg/apimsg.c msg/aplmsg.c \
	msg/autooutmsg.c msg/clrmsg.c msg/getsmsg.c msg/inimsg.c \
	msg/inquiremsg.c msg/outmsg.c msg/pltmsg.c msg/sacmsg.c \
	msg/sendmesg.c msg/setmsg.c msg/typmsg.c msg/wrtmsg.c \
	\
	ncpf/appendstring.c ncpf/changestring.c ncpf/closemacro.c \
	ncpf/deletestring.c ncpf/executemacro.c \
	ncpf/getepoch.c ncpf/getmacroinfo.c \
	ncpf/gettime.c  ncpf/indexs.c ncpf/inicpf.c \
	ncpf/macrokeyword.c ncpf/macroline.c ncpf/macroordered.c \
	ncpf/macropreambl.c ncpf/macrostatus.c ncpf/nstrlensp.c \
	ncpf/openmacro.c ncpf/prependstring.c \
	ncpf/setmacrolev.c \
	ncpf/setmacrostat.c \
	\
	nnm/xnnmc.c nnm/xwritenn.c \
	\
	pl/centxt.c pl/incat.c pl/inicol.c pl/inigem.c pl/inilin.c \
	pl/iniwidth.c pl/logdta.c pl/pl2d.c pl/plalpha.c pl/plblank.c \
	pl/plcalwvtrans.c pl/plclip.c pl/pldta.c pl/plgrid.c pl/plhome.c \
	pl/plmap.c pl/plnocl.c pl/plplab.c pl/plrest.c pl/plsave.c \
	pl/pltext.c pl/xlinax.c pl/xlogax.c pl/ylinax.c pl/ylogax.c \
	\
	sam/chkpha.c sam/cpft.c sam/estpha.c sam/fdWhitenWrite.c \
	sam/fdWriteFiles.c sam/fdbp.c sam/fdbr.c sam/fdhp.c sam/fdlp.c \
	sam/fdplot.c sam/filtb.c sam/filterdesign.c sam/filtk.c sam/inisam.c \
	sam/rfir.c sam/spcval.c sam/toamph.c sam/torlim.c sam/unwrap.c \
	sam/xbenioff.c sam/xbp.c sam/xbr.c sam/xconvolve.c sam/xcorrelate.c \
	sam/xdft.c sam/xdivomega.c sam/xenvelope.c sam/xfir.c sam/xhan.c \
	sam/xhilbert.c sam/xhp.c sam/xidft.c sam/xkeepam.c sam/xkhronhite.c \
	sam/xlp.c sam/xmulomega.c sam/xpsp.c sam/xrsp.c sam/xsamc.c \
	sam/xunwr.c sam/xwnr.c sam/xwsp.c sam/envelope.c \
	\
	scm/iniscm.c scm/lifite.c \
	scm/lifitu.c scm/linear.c scm/rotate.c scm/wigint.c scm/xabsgl.c \
	scm/xdecimate.c scm/xinterpolate.c scm/xlinefit.c scm/xpowgl.c \
	scm/xquantize.c scm/xreverse.c scm/xrglitches.c scm/xrmean.c \
	scm/xrotate.c scm/xrq.c scm/xrtr.c scm/xscmc.c scm/xsmooth.c \
	scm/xstretch.c scm/xtaper.c \
	scm/rmean.c scm/rtrend.c scm/taper.c scm/interpolate.c \
	\
	site/initsite.c site/xsitecom.c site/xtestsite.c \
	\
	smm/inismm.c smm/ptp.c smm/xmarkptp.c \
	smm/xmarktimes.c smm/xmarkvalue.c smm/xrms.c smm/xsmmc.c \
	\
	spe/inispe.c spe/xcor.c spe/xmem.c spe/xmlm.c spe/xpcor.c \
	spe/xpds.c spe/xppe.c spe/xpspe.c spe/xquitspe.c spe/xrcor.c \
	spe/xspe.c spe/xspec.c spe/xwcor.c spe/xwhiten.c spe/xwspe.c \
	\
	ssi/CSStoSAC.c ssi/DBheaderToSac.c ssi/DBwfToSac.c ssi/OnOrOff.c \
	ssi/SacHeaderToDB.c ssi/SeisMgrCode.c ssi/SeisMgrToSac.c \
	ssi/alignFiles.c ssi/deleteAllSacFiles.c ssi/inissi.c \
	ssi/prefPicksToHeader.c ssi/rollback.c ssi/sacToSeisMgr.c \
	ssi/uniqueStaAndChan.c ssi/xblend.c ssi/xcommit.c ssi/xcutim.c \
	ssi/xreaddb.c ssi/xrollback.c ssi/xtablname.c \
	\
	sss/definelimits.c sss/iaspmodel.c sss/inisss.c sss/libtau.c \
	sss/phaseadj.c sss/rscursor.c sss/timeadj.c sss/timecrossing.c \
	sss/ttint.c sss/velocityadj.c sss/vmcalc.c sss/vmdly.c sss/vmline.c \
	sss/xaddstack.c sss/xchangestack.c sss/xdeletestack.c \
	sss/xdeltacheck.c sss/xdistanceaxis.c sss/xdistancewind.c \
	sss/xglobalstack.c sss/xincrementsta.c sss/xliststack.c \
	sss/xphase.c sss/xplotrecords.c sss/xplotstack.c sss/xquitsss.c \
	sss/xsss.c sss/xsssc.c sss/xsumstack.c sss/xtimeaxis.c \
	sss/xtimewindow.c sss/xtraveltime.c sss/xvelocitymode.c \
	sss/xvelocityrose.c sss/xwritestack.c sss/xzerostack.c \
	sss/ttlim.h \
	\
	top/executecomman.c top/findcommand.c top/initcommon.c top/initsac.c \
	top/saccommands.c \
	\
	ucf/basenm.c ucf/byteswap.c ucf/cnvatf.c ucf/cnvati.c ucf/cnvfta.c \
	ucf/cnvita.c ucf/copydouble.c ucf/copyi.c ucf/copykc.c \
	ucf/ddttm.c ucf/distaz.c ucf/evallogical.c ucf/extrma.c \
	ucf/fill.c ucf/getxw.c ucf/getyw.c ucf/idttm.c ucf/idttmf.c \
	ucf/incdat.c ucf/inctim.c ucf/inctimf.c ucf/initok.c ucf/kadate.c \
	ucf/kadttm.c ucf/katime.c ucf/kidate.c ucf/kijdat.c ucf/lbsrch.c \
	ucf/ldttm.c ucf/linrng.c ucf/ljust.c ucf/nccomp.c ucf/next2.c \
	ucf/ophelp.c ucf/poptok.c ucf/sorti.c ucf/srtndx.c ucf/timecheck.c \
	ucf/tokdel.c ucf/tosecs.c ucf/wapf.c ucf/wrcom.c ucf/wrhelp.c \
	ucf/wrindx.c \
	\
	uom/dif2.c uom/dif3.c uom/dif5.c uom/iniuom.c uom/xabs.c uom/xadd.c \
	uom/xdif.c uom/xdiv.c uom/xexp.c uom/xexp10.c uom/xint.c uom/xlog.c \
	uom/xlog10.c uom/xmul.c uom/xsqr.c uom/xsqrt.c uom/xsub.c uom/xuomc.c \
	\
  vars/createvlist.c \
	vars/deletev.c vars/deletevlist.c \
	vars/getvFILEptr.c \
	vars/getvvstring.c \
	vars/initializevar.c \
	vars/inivars.c vars/putvFILEptr.c \
	vars/putvvstring.c vars/readvfile.c \
	vars/writevfile.c \
	vars/var.c vars/chash.c vars/bbf.c \
	\
	wild/getdir.c wild/lwildc.c wild/wildch.c \
	wild/wildfl.c \
	\
	xyz/adjust_height.c xyz/adjust_width.c xyz/calcfftsize.c \
	xyz/calcloc.c xyz/calcsize.c xyz/cbar_window.c xyz/flipdata.c \
	xyz/getdata.c xyz/inixyz.c xyz/label_cbar.c xyz/linear_interp.c \
	xyz/plotimage.c xyz/scaleimage.c xyz/scallop.c xyz/smooth.c \
	xyz/spcgrm.c xyz/specplot.c xyz/spectrogram.c xyz/subtract.c \
	xyz/vfxyz.c xyz/window_data.c xyz/writezdata.c xyz/xcontour.c \
	xyz/xgrayscale.c xyz/ximage.c xyz/xscallop.c xyz/xspectrogram.c \
	xyz/xxyzc.c xyz/xyzcleanup.c xyz/xzcolors.c xyz/xzlabels.c \
	xyz/xzlevels.c xyz/xzlines.c xyz/xzticks.c xyz/zfft.c


libsac_check_a_SOURCES = ucf/cnvatf.c ucf/cnvati.c ucf/inctim.c ucf/inctimf.c \
	                 ucf/cnvfta.c ucf/copydouble.c

libsac_x11_a_SOURCES = 	\
	gd3.x11/begindevice3.c gd3.x11/beginframe3.c gd3.x11/beginwindow3.c \
	gd3.x11/calc_loc3.c gd3.x11/cbar_window3.c gd3.x11/changectable3.c \
	gd3.x11/createwindo3.c gd3.x11/cursor3.c gd3.x11/cursortext3.c \
	gd3.x11/dispatcheve3.c gd3.x11/draw3.c gd3.x11/drawpoly3.c \
	gd3.x11/enddevice3.c gd3.x11/endframe3.c gd3.x11/erase3.c \
	gd3.x11/error3.c gd3.x11/expose3.c gd3.x11/fill_clrbar3.c \
	gd3.x11/fill_image3.c gd3.x11/flushbuffer3.c gd3.x11/get_geometry3.c \
	gd3.x11/getalphainf3.c gd3.x11/getdevicein3.c gd3.x11/getdevicera3.c \
	gd3.x11/getratio3.c gd3.x11/getwindowst3.c gd3.x11/initdevice3.c \
	gd3.x11/make_label3.c gd3.x11/move3.c gd3.x11/put_image3.c \
	gd3.x11/setcolor3.c gd3.x11/setctable3.c gd3.x11/setlinestyl3.c \
	gd3.x11/setpsctable3.c gd3.x11/settextsize3.c gd3.x11/setwidth3.c \
	gd3.x11/save3.c 

libsgf_a_SOURCES = gd2/beginSGFtemp.c \
	gd2/begindevice2.c gd2/beginframe2.c gd2/beginwindow2.c \
	gd2/calc_loc2.c gd2/cbar_window2.c gd2/changectable2.c \
	gd2/createwindo2.c gd2/cursor2.c gd2/cursortext2.c gd2/draw2.c \
	gd2/endSGFtemp.c gd2/enddevice2.c gd2/endframe2.c gd2/erase2.c \
	gd2/fill_clrbar2.c gd2/fill_image2.c gd2/flushbuffer2.c \
	gd2/get_geometry2.c gd2/getalphainf2.c gd2/getdevicein2.c \
	gd2/getdevicera2.c gd2/getratio2.c gd2/getwindowst2.c \
	gd2/hardwaretex2.c gd2/initdevice2.c gd2/move2.c gd2/put_image2.c \
	gd2/setcolor2.c gd2/setctable2.c gd2/setlinestyl2.c gd2/setsgfdir.c \
	gd2/setsgfnumber.c gd2/setsgfprefix.c gd2/setsgfsize.c \
	gd2/settextangl2.c gd2/settextsize2.c gd2/setwidth2.c 


libturtle_a_SOURCES    = docs/pdf.c docs/ps.c docs/font.c string/strings.c \
	string/array.c string/array.h \
	string/datetime.c \
	docs/Courier-Bold.h docs/Courier-BoldOblique.h	\
	docs/Courier-Oblique.h docs/Courier.h \
	docs/font.h \
	docs/Helvetica-Bold.h docs/Helvetica-BoldOblique.h \
	docs/Helvetica-Oblique.h docs/Helvetica.h \
	docs/pdf.h \
	docs/ps.h \
	docs/Symbol.h \
	docs/Times-Bold.h docs/Times-BoldItalic.h \
	docs/Times-Italic.h docs/Times-Roman.h \
	docs/ZapfDingbats.h \
	eval/expr_lexer.c eval/expr_parse.c eval/expr_util.c eval/expr_parse_noop.c \
	eval/eval.h eval/expr_parse.h

# sacrc related files
# Status: Unknown as of 101.2
#	\
#	rc/rc_use.c rc/rc_options.c rc/rc.lex.c rc/rc.tab.c

