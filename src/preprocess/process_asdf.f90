module process_asdf_mod

contains
!> \Time-series Analysis for global adjoint tomography
!! Developer: Princeton Global Tomography Group(PGTG)
!! Group Member: James Smith jas11(@princeton.edu), Ebru Bozdag(bozdag@princeton.edu),
!! Wenjie Lei (lei@princeton.edu)
!! Bug Report: jas11@princeton.edu

subroutine process_asdf(observed_raw, synthetic_raw, observed_rotate, synthetic_rotate, rank, comm)

  use mpi
  use asdf_data
  use asdf_read_subs
  use process_var
  use process_subs
  use process_subs2
  use var_main

  implicit none

  integer                   :: comm, rank, ierr, nproc, adios_err
  integer(kind=8)           :: adios_handle, adios_group
  integer(kind=8)           :: adios_groupsize, adios_totalsize

  type (asdf_event),intent(inout) :: observed_raw, synthetic_raw
  type (asdf_event),intent(inout) :: observed_rotate, synthetic_rotate
  type (asdf_event)               :: observed_proc, synthetic_proc

  !local trace variables(In accordance with SAC lib, the data type is real)
  real(kind=4) :: b_obsd, b_synt, dt_obsd, dt_synt
  integer :: npts_obsd, npts_synt
  real(kind=4) :: obsd(MAX_TRACE_LENGTH), synt(MAX_TRACE_LENGTH)

  !after cut
  real(kind=4) :: b_obsd_cut, b_synt_cut, dt_obsd_cut, dt_synt_cut
  integer :: npts_obsd_cut, npts_synt_cut
  real(kind=4) :: obsd_cut(MAX_TRACE_LENGTH), synt_cut(MAX_TRACE_LENGTH)

  !remove trend and mean
  integer :: ipts
  real :: mean, yint, slope

  !instrument response
  character(len=20) :: sta, net, locid, cha

  !after interpolation
  real(kind=4) :: b_obsd_interp, b_synt_interp, dt_obsd_interp, dt_synt_interp
  integer :: npts_obsd_interp, npts_synt_interp
  real(kind=4) :: obsd_interp(MAX_TRACE_LENGTH), synt_interp(MAX_TRACE_LENGTH)
  real :: end_time

  !after final cut
  real(kind=4) :: b_obsd_final, b_synt_final, dt_obsd_final, dt_synt_final
  integer :: npts_obsd_final, npts_synt_final
  real(kind=4) :: obsd_final(MAX_TRACE_LENGTH), synt_final(MAX_TRACE_LENGTH)

  integer :: nerr

!-----------------------------------------------------------------------------------
! READ CMT FILE AND COMPUTE JULIAN DAY AND ORIGIN OF EVENT
!-----------------------------------------------------------------------------------
  call read_CMT(observed_raw%event, rank, comm, ierr)
  call event_origin(rank, comm, ierr)

!-----------------------------------------------------------------------------------
! ADJUST TIME OF OBSERVED DATA BASED ON CMT EVENT ORIGIN
!-----------------------------------------------------------------------------------
  call adjust_event_time(observed_raw, ierr)
  print *, "observed origin time adjusted based on cmt event"

!-----------------------------------------------------------------------------------
! LOOOP THROUGH PAIRS of OBSERVED AND SYNTHETIC TRACES
!-----------------------------------------------------------------------------------
  call init_asdf_data(observed_proc, observed_raw%nrecords, .false.)
  call init_asdf_data(synthetic_proc, synthetic_raw%nrecords, .false.)
  call init_asdf_data(observed_rotate, observed_raw%nrecords, .false.)
  call init_asdf_data(synthetic_rotate, synthetic_raw%nrecords, .false.)

  do irecord = 1, observed_raw%nrecords
    print *, "Processing record ", irecord, " of ", observed_raw%nrecords, " on processor " , rank, "."
    print *, "Station name: ", trim(observed_raw%receiver_name_array(irecord))
    print *, "Component name: ", trim(observed_raw%component_array(irecord))

    !if (synt_raw%npoints(irecord) .gt. 1) then

!-----------------------------------------------------------------------------------
! CUT OBSERVED AND SYNTHETIC SEISMOGRAM
!-----------------------------------------------------------------------------------
    print *, "First cut begin..."
    !copy trace info to local var
    b_obsd=sngl(observed_raw%begin_value(irecord))
    dt_obsd=sngl(observed_raw%sample_rate(irecord))
    npts_obsd=observed_raw%npoints(irecord)
    obsd(1:npts_obsd)=sngl(observed_raw%records(irecord)%record(1:npts_obsd))

    b_synt=sngl(synthetic_raw%begin_value(irecord))
    dt_synt=sngl(synthetic_raw%sample_rate(irecord))
    npts_synt=synthetic_raw%npoints(irecord)
    synt(1:npts_synt)=sngl(synthetic_raw%records(irecord)%record(1:npts_synt))

    sta=observed_raw%receiver_name_array(irecord)
    net=observed_raw%network_array(irecord)
    locid=observed_raw%receiver_id_array(irecord)
    cha=observed_raw%component_array(irecord)

    call wsac1("adjust_event_time.obs", obsd, npts_obsd, &
          b_obsd, dt_obsd, nerr)
    call wsac1("adjust_event_time.syn", synt, npts_synt, &
          b_synt, dt_synt, nerr)

    call cut_seis(obsd, npts_obsd, dt_obsd, b_obsd, synt, npts_synt, dt_synt, b_synt, &
    obsd_cut, npts_obsd_cut, dt_obsd_cut, b_obsd_cut, &
    synt_cut, npts_synt_cut, dt_synt_cut, b_synt_cut)
    print *, "first cut done..."

    call wsac1("cut.obs", obsd_cut, npts_obsd_cut, &
          b_obsd_cut, dt_obsd_cut, nerr)
    call wsac1("cut.syn", synt_cut, npts_synt_cut, &
          b_synt_cut, dt_synt_cut, nerr)

!-----------------------------------------------------------------------------------
! PREPARE FOR DECONVOLUTION: rmean/rtrend/taper
!-----------------------------------------------------------------------------------
    mean=0
!   slope=0
    yint=0
    !call rmean(obsd_cut, npts_obsd_cut, mean)
    call demean(obsd_cut, npts_obsd_cut, mean)
    call rtrend(obsd_cut, npts_obsd_cut, yint, slope, &
            b_obsd_cut, dt_obsd_cut)
    call taper_width_to_points(.05, npts_synt_cut, ipts)
    call taper(obsd_cut, npts_obsd_cut, 2, ipts)

    print *, "mean:", sum(obsd_cut(1:npts_obsd_cut))/npts_obsd_cut
    print *, "npts:", npts_obsd_cut
    print *, "mean, yint, slope:", mean, yint, slope

    mean=0
    slope=0
    yint=0
    !call rmean(synt_cut, npts_synt_cut, mean)
    call demean(synt_cut, npts_synt_cut, mean)
    call rtrend(synt_cut, npts_synt_cut, yint, slope, &
             b_synt_cut, dt_synt_cut)
    call taper_width_to_points(.05, npts_synt_cut, ipts)
    call taper(synt_cut, npts_synt_cut, 2, ipts)

    print *, "npts:", npts_synt_cut
    print *, "mean, yint, slope:", mean, yint, slope

    call wsac1("rmean.obs", obsd_cut, npts_obsd_cut, &
          b_obsd_cut, dt_obsd_cut, nerr)
    call wsac1("rmean.syn", synt_cut, npts_synt_cut, &
          b_synt_cut, dt_synt_cut, nerr)

    print *, "prepared for deconvolution"


    call wsac1("before_filter.syn", synt_cut, npts_synt_cut, &
          b_synt_cut, dt_synt_cut, nerr)
!-----------------------------------------------------------------------------------
! REMOVE INSTRUMENT RESPONSE
!-----------------------------------------------------------------------------------
    call remove_response(obsd_cut, npts_obsd_cut, dt_obsd_cut, &
      sta, net, cha, locid, observed_raw%responses(irecord)%response_string, &
      observed_raw%responses(irecord)%response_length)
    call filter(synt_cut, npts_synt_cut, dt_synt_cut)

    !call wsac1("filter.obs", obsd_cut, npts_obsd_cut, &
    !      b_obsd_cut, dt_obsd_cut, nerr)
    call wsac1("filter.syn", synt_cut, npts_synt_cut, &
          b_synt_cut, dt_synt_cut, nerr)
    print *, "removed response"
!
!-----------------------------------------------------------------------------------
! rmean/rtrend/taper again in case instrument response introduces artifacts
!-----------------------------------------------------------------------------------

!    call rmean(cut_observed, npoints_observed, mean)
!    call rtrend(cut_observed, npoints_observed, yint, slope, &
!                beg_max, sngl(observed_raw%sample_rate(irecord)))
!    call taper_width_to_points(.05, npoints_synthetic, ipts)
!    call taper(cut_observed, npoints_observed, 2, ipts)
!
!    call rmean(cut_synthetic, npoints_synthetic, mean)
!    call rtrend(cut_synthetic, npoints_synthetic, yint, slope, &
!                beg_max, sngl(synthetic_raw%sample_rate(irecord)))
!    call taper_width_to_points(.05, npoints_synthetic, ipts)
!    call taper(cut_synthetic, npoints_synthetic, 2, ipts)
!    print *, "prepared for deconvolution"

!-----------------------------------------------------------------------------------
! RESAMPLE OBSERVED AND SYNTHETIC SEISMOGRAMS TO SAME SAMPLE RATE
!-----------------------------------------------------------------------------------

    end_time=b_synt_cut+dt_synt_cut*(npts_synt_cut-1)
    dt_synt_interp=1.0
    b_synt_interp=b_synt_cut
    npts_synt_interp=10000
    print *, "end_time:", end_time
    call interp(synt_cut, npts_synt_cut, synt_interp, npts_synt_interp,&
                b_synt_cut, end_time, dt_synt_cut, &
                b_synt_interp, dt_synt_interp, error)
!    call interp(obsd_cut, npts_obsd_cut, obsd_interp, npts_obsd_interp,&
!                beg_max, end_min, dt_obsd_cut, &
!                b_obsd_interp, dt_obsd_interp, error)
!    print *, "resample complete"
    call wsac1("final.syn", synt_interp, npts_synt_interp,&
          b_synt_interp, dt_synt_interp, nerr)
    !call wsac1("cut.syn", cut_synthetic, npoints_synthetic,&
    !         beg, sngl(synthetic_raw%sample_rate(irecord)), nerr)

!-----------------------------------------------------------------------------------
! FINAL CUT OF OBSERVED AND SYNTHETIC SEISMOGRAMS
!-----------------------------------------------------------------------------------

!    call cut_seis(obsd_interp, npts_obsd_interp, dt_obsd_interp, b_obsd_interp, &
!      synt_cut, npts_synt_interp, dt_synt_interp, b_synt_interp, &
!      obsd_final, npts_obsd_final, dt_obsd_final, b_obsd_final, &
!      synt_final, npts_synt_final, dt_synt_final, b_synt_final)
!    print *, "final cut finished"
!    
!    observed_final%begin_value(irecord)=b_obsd_final
!    observed_final%sample_rate(irecord)=dt_obsd_final
!    observed_final%npoints(irecord)=npts_obsd_final
!    allocate(observed_final%records(irecord)%record(npts_obsd_final))
!    observed_final%records(irecord)%record(1:npts_obsd_final)=dble(obsd_final(1:npts_obsd_final))
!
!    synthetic_final%begin_value(irecord)=b_synt_final
!    synthetic_final%sample_rate(irecord)=dt_synt_final
!    synthetic_final%npoints(irecord)=npts_synt_final
!    allocate(synthetic_final%records(irecord)%record(npts_synt_final))
!    synthetic_final%records(irecord)%record(1:npts_synt_final)=dble(obsd_final(1:npts_synt_final))
!    !endif
  enddo

!-----------------------------------------------------------------------------------
! COPY METADATA TO ASDF CONTAINER
!-----------------------------------------------------------------------------------
!  call store_asdf_metadata(observed_raw, observed_final)
!  call store_asdf_metadata(synthetic_raw, synthetic_final)

!-----------------------------------------------------------------------------------
! ROTATE
!-----------------------------------------------------------------------------------
!  call rotate_traces(observed_final, observed_rotate)
!  call rotate_traces(synthetic_final, synthetic_rotate)
!  print *, "rotation finished"

!  call deallocate_asdf(observed_raw)
!  call deallocate_asdf(synthetic_raw)

end subroutine process_asdf

end module
