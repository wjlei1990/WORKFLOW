module flexwin_interface_subs

  implicit none

contains

subroutine read_flexwin_parfile_mpi(flexwin_par_all, min_period, &
                      max_period, event_dpt, nrecords, &
											rank, comm, ierr)

	use flexwin_struct
  use mpi

	type(flexwin_par_struct_all) :: flexwin_par_all
  double precision :: min_period, max_period, event_dpt(:)
	integer :: rank, comm, ierr
  integer :: nrecords

	integer :: oldtype(6), newtype, offset(6), blockcount(6)
	integer :: extent

	integer :: tag=1, i, source, loc
	integer :: stat(MPI_STATUS_SIZE)

	character(len=150) :: fn

  double precision :: aver_event_dpt


  aver_event_dpt=sum(event_dpt(1:nrecords))/nrecords
  !check if the event_dpt are simliar
  !cause current version of FLEXWIN can only deal with asdf from one earthquake
  if(abs(aver_event_dpt-event_dpt(1)).gt.5.0) then
    print *, "Check if the event_dpt(:) array are the same"
    print *, "This version FLEXWIN only takes asdf from one earthquake"
    stop
  endif
	!print *,"SET UP"
	!setup description of the flexwin_par
	!call read_flexwin_parfile_mpi(flexwin_par, fstart, fend, rank, nproc, comm)

  do i=1,3
    loc=2*i-1
    if(loc.eq.1)then
      offset(loc)=0
    else
      call MPI_TYPE_EXTENT(MPI_DOUBLE_PRECISION, extent, ierr)
	    offset(loc) = offset(loc-1)+23*extent
    endif
	  oldtype(loc) = MPI_LOGICAL
	  blockcount(loc) = 6

   	loc=2*i
	  call MPI_TYPE_EXTENT(MPI_LOGICAL, extent, ierr)
	  offset(loc) = offset(loc-1)+6*extent
	  oldtype(loc) = MPI_DOUBLE_PRECISION
	  blockcount(loc) = 23
		!if(rank.eq.0) then
		!	print *,"blockcount",loc,blockcount(loc)
		!endif
		!if(rank.eq.1) then
		!	print *,"blockcount:",blockcount(loc)
		!endif

		!print *,i, blockcount(1), blockcount(2), blockcount(3), blockcount(4),&
		!					blockcount(5), blockcount(6)
  enddo

	!now define and commit
	call MPI_TYPE_STRUCT(6, blockcount, offset, oldtype, newtype, ierr)
	call MPI_TYPE_COMMIT(newtype, ierr)

	!print *, "SET UP finished!"

	if(rank.eq.0) then
    print *,"Period Band(s):", min_period, max_period
    print *,"event_dpt:", aver_event_dpt
		call read_flexwin_parfile(flexwin_par_all, min_period, &
                      max_period, aver_event_dpt)
	endif

  call MPI_Bcast(flexwin_par_all, 1, newtype, 0, comm, ierr)
	
	!if(rank==1) then
	!	print *, flexwin_par_all%T%DEBUG
	!	print *, flexwin_par_all%T%CC_BASE
	!	print *, flexwin_par_all%T%WEIGHT_N_WINDOWS
	!endif
	
	print *, "Finalize reading flexwin parfile"

end subroutine read_flexwin_parfile_mpi


  !>read flexwin parameter file
  !!read in three component and store them into flexwin_par_all
  subroutine read_flexwin_parfile(flexwin_par_all, fstart, fend, event_dpt)

    use flexwin_struct 
    implicit none

    !here, fstart and fend are actually the period(min_period, max_period) here
    type(flexwin_par_struct_all) :: flexwin_par_all
    double precision :: fstart, fend, event_dpt

    integer :: fstart_int, fend_int
    character(len=20) :: fstart_string, fend_string

    integer :: i
    character(len=150) :: parfile_dir, fn
    
    !print *, fstart, fend
    write(fstart_string,'(i10)')int(fstart)
    write(fend_string,'(i10)')int(fend)
    !print *, fstart_string, fend_string
    fstart_string=adjustl(fstart_string)
    fend_string=adjustl(fend_string)
    
    parfile_dir="./PAR_FILES"

    !read in three component into flexwin_par_all
    fn=trim(parfile_dir)//"/PAR_FILE_"//trim(fstart_string)//"_"//&
              trim(fend_string)//"_Z"
    print *, "Reading in flexwin par in Z component"
    print *, "parfile name:", trim(fn)
    call read_flexwin_parfile_comp(fn, flexwin_par_all%Z)
    !fn="./FLEXWIN/PAR_FILE"
    fn=trim(parfile_dir)//"/PAR_FILE_"//trim(fstart_string)//"_"//&
              trim(fend_string)//"_R"
    print *, "Reading in flexwin par in R component"
    print *, "parfile name:", trim(fn)
    call read_flexwin_parfile_comp(fn, flexwin_par_all%R)
    !fn="./FLEXWIN/PAR_FILE"
    fn=trim(parfile_dir)//"/PAR_FILE_"//trim(fstart_string)//"_"//&
              trim(fend_string)//"_T"
    print *, "Reading in flexwin par in T component"
    print *, "parfile name:", trim(fn)
    call read_flexwin_parfile_comp(fn, flexwin_par_all%T)

    !stop

  end subroutine read_flexwin_parfile

  subroutine read_flexwin_parfile_comp(fn, flexwin_par)

    use flexwin_struct 
    use seismo_variables
    implicit none

    character(len=*) :: fn
    type(flexwin_par_struct) :: flexwin_par

    integer, parameter :: IIN = 11
    integer, parameter :: NHEAD = 12

    integer :: idummy
    character(len=34) junk

    open(unit=IIN,file=fn,status='old')

    ! ignore header
    do idummy=1,NHEAD
      read(IIN,*)
    enddo

    !--------------------------------------------------------
    ! read parameters, skipping empty lines and comment lines

    ! boolean parameters
    read(IIN,*)
    read(IIN,*)
    read(IIN,*)
    read(IIN,3) junk,flexwin_par%DEBUG
    read(IIN,3) junk,flexwin_par%MAKE_SEISMO_PLOTS
    read(IIN,3) junk,flexwin_par%MAKE_WINDOW_FILES
    read(IIN,3) junk,flexwin_par%BODY_WAVE_ONLY
    if (flexwin_par%DEBUG) then
      write(*,*) 'DEBUG: ------------------------------------:'
      write(*,*) 'DEBUG: PARAMETERS read in from PAR_FILE are:'
      write(*,*) '       DEBUG',flexwin_par%DEBUG
      write(*,*) '       MAKE_SEISMO_PLOTS',flexwin_par%MAKE_SEISMO_PLOTS
      write(*,*) '       MAKE_WINDOW_FILES',flexwin_par%MAKE_WINDOW_FILES
      write(*,*) '       BODY_WAVE_ONLY',flexwin_par%BODY_WAVE_ONLY
    endif

    ! period min/max for filtering
    read(IIN,*)
    read(IIN,*)
    read(IIN,*)
    read(IIN,3) junk,flexwin_par%RUN_BANDPASS
    read(IIN,2) junk,flexwin_par%WIN_MIN_PERIOD
    read(IIN,2) junk,flexwin_par%WIN_MAX_PERIOD
    FSTART = 1./flexwin_par%WIN_MAX_PERIOD
    FEND   = 1./flexwin_par%WIN_MIN_PERIOD
    if (flexwin_par%DEBUG) then
      write(*,*) '       WIN_MIN_PERIOD',flexwin_par%WIN_MIN_PERIOD
      write(*,*) '       WIN_MAX_PERIOD',flexwin_par%WIN_MAX_PERIOD
    endif

    ! E(t) water level
    read(IIN,*)
    read(IIN,*)
    read(IIN,*)
    read(IIN,2) junk,flexwin_par%STALTA_BASE
    if (flexwin_par%DEBUG) write(*,*) '       STALTA_BASE',flexwin_par%STALTA_BASE

    ! Tshift
    read(IIN,*)
    read(IIN,*)
    read(IIN,*)
    read(IIN,2) junk,flexwin_par%TSHIFT_BASE
    read(IIN,2) junk,flexwin_par%TSHIFT_REFERENCE
    if (flexwin_par%DEBUG) write(*,*) '       TSHIFT_BASE',flexwin_par%TSHIFT_BASE
    if (flexwin_par%DEBUG) write(*,*) '       TSHIFT_REFERENCE',flexwin_par%TSHIFT_REFERENCE

    ! limit on dlnA for window acceptance
    read(IIN,*)
    read(IIN,*)
    read(IIN,*)
    read(IIN,2) junk,flexwin_par%DLNA_BASE
    read(IIN,2) junk,flexwin_par%DLNA_REFERENCE
    if (flexwin_par%DEBUG) write(*,*) '       DLNA_BASE',flexwin_par%DLNA_BASE
    if (flexwin_par%DEBUG) write(*,*) '       DLNA_REFERENCE',flexwin_par%DLNA_REFERENCE

    ! limit on CC
    read(IIN,*)
    read(IIN,*)
    read(IIN,*)
    read(IIN,2) junk,flexwin_par%CC_BASE
    if (flexwin_par%DEBUG) write(*,*) '       CC_BASE',flexwin_par%CC_BASE

    ! boolean switch for check_data_quality
    read(IIN,*)
    read(IIN,*)
    read(IIN,*)
    read(IIN,3) junk,flexwin_par%DATA_QUALITY

    ! if DATA_QUALITY = .true. and if two different measurements of
    ! signal-to-noise ratios exceeds these two base levels,
    ! then the data time series (and syn) is kept
    read(IIN,*)
    read(IIN,*)
    read(IIN,*)
    read(IIN,*)
    read(IIN,2) junk,flexwin_par%SNR_INTEGRATE_BASE
    read(IIN,2) junk,flexwin_par%SNR_MAX_BASE

    ! limit on signal to noise ratio in a particular window.
    read(IIN,*)
    read(IIN,*)
    read(IIN,*)
    read(IIN,2) junk,flexwin_par%WINDOW_S2N_BASE

    ! Fine tuning constants
    read(IIN,*)
    read(IIN,*)
    read(IIN,*)
    read(IIN,2) junk,flexwin_par%C_0
    read(IIN,2) junk,flexwin_par%C_1
    read(IIN,2) junk,flexwin_par%C_2
    read(IIN,2) junk,flexwin_par%C_3a
    read(IIN,2) junk,flexwin_par%C_3b
    read(IIN,2) junk,flexwin_par%C_4a
    read(IIN,2) junk,flexwin_par%C_4b
    read(IIN,*)
    read(IIN,2) junk,flexwin_par%WEIGHT_SPACE_COVERAGE
    read(IIN,2) junk,flexwin_par%WEIGHT_AVERAGE_CC
    read(IIN,2) junk,flexwin_par%WEIGHT_N_WINDOWS

    if (flexwin_par%DEBUG) then
      write(*,*) 'DEBUG: ------------------------------------:'
    endif

    !--------------------------------------------------------
    ! close parameter file
    close(IIN)


    !--------------------------------------------------------
    ! line formats
2   format(a,f20.8)
3   format(a,l20)

    ! unused formats
    ! 1 format(a,i20)
    ! 4 format(a,a)

  end subroutine
!----------------------------------------------------------------------

end module flexwin_interface_subs 
