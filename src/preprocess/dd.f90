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
  use rotate_subs
  use var_main

  integer                   :: comm, rank, ierr, nproc, adios_err
  integer(kind=8)           :: adios_handle, adios_group
  integer(kind=8)           :: adios_groupsize, adios_totalsize

  type (asdf_event),intent(inout) :: observed_raw, synthetic_raw
  type (asdf_event) :: observed_proc, synthetic_proc
  type (asdf_event),intent(inout) :: observed_rotate, synthetic_rotate

!-----------------------------------------------------------------------------------
! READ CMT FILE AND COMPUTE JULIAN DAY AND ORIGIN OF EVENT
!-----------------------------------------------------------------------------------

  call read_CMT(observed_raw%event, rank, comm, ierr)
  call event_origin(rank, comm, ierr)

!-----------------------------------------------------------------------------------
! LOOOP THROUGH PAIRS of OBSERVED AND SYNTHETIC TRACES
!-----------------------------------------------------------------------------------

  do irecord = 1, observed_raw%nrecords
    print *, "Processing record ", irecord, " of ", observed_raw%nrecords, " on processor " , rank, "."
    print *, "Station name: ", observed_raw%receiver_name_array(irecord)
    print *, "Component name: ", observed_raw%component_array(irecord)

    if (synthetic_raw%npoints(irecord) .gt. 1) then

!-----------------------------------------------------------------------------------
! ADJUST TIME OF OBSERVED DATA BASED ON CMT EVENT ORIGIN
!-----------------------------------------------------------------------------------

      call adjust_event_time(observed_raw, irecord)
      print *, "observed origin time adjusted based on cmt event"

!-----------------------------------------------------------------------------------
! CUT OBSERVED AND SYNTHETIC SEISMOGRAM
!-----------------------------------------------------------------------------------

    ! generate time arrays
    allocate(time_observed(observed_raw%npoints(irecord)))
    do i = 1, observed_raw%npoints(irecord)
      time_observed(i) = observed_raw%begin_value(irecord) + observed_raw%sample_rate(irecord)*dble(i-1)
    enddo
    allocate(time_synthetic(synthetic_raw%npoints(irecord)))
    do i = 1, synthetic_raw%npoints(irecord)
      time_synthetic(i) = synthetic_raw%begin_value(irecord) + synthetic_raw%sample_rate(irecord)*dble(i-1)
    enddo

    ! define larger of observed and synthetic begin time
    beg_max=max(sngl(observed_raw%begin_value(irecord)),sngl(synthetic_raw%begin_value(irecord)))+1
    ! define smaller of observed and synthetic end time
    end_min=min(sngl(time_observed(observed_raw%npoints(irecord))),sngl(time_synthetic(synthetic_raw%npoints(irecord))))-1
    ! Define cut for observed trace
    call cut_define(beg_max, sngl(observed_raw%sample_rate(irecord)), end_min, npoints_cut_observed)
    ! Define npoints_start and npoints_end
    j = 0
    jj = 0
    k = time_observed(1)
    kk = time_observed(1)
    do i = 1, observed_raw%npoints(irecord)
      if (k .le. end_min) then
        j = j + 1
        k = time_observed(1) + observed_raw%sample_rate(irecord)*dble(j-1)
      endif
      if (kk .le. beg_max) then
        jj = jj + 1
        kk = time_observed(1) + observed_raw%sample_rate(irecord)*dble(jj-1)
      endif
    enddo
    npoints_start = jj -1
    npoints_end = j
    call cut_define_check(beg_max, end_min, npoints_cut_observed, cuterr,&
        npoints_start, npoints_end, nfillb, nfille, nerr)
    cut_length = npoints_end - npoints_start + 1
    allocate(cut_observed(cut_length))

    ! Cut observed trace
    call cut(sngl(observed_raw%records(irecord)%record), npoints_start, npoints_end,&
              nfillb, nfille, cut_observed)
    npoints_observed = size(cut_observed)

    ! Define cut for synthetic trace
    call cut_define(beg_max, sngl(synthetic_raw%sample_rate(irecord)), end_min, npoints_cut_synthetic)

    ! Define npoints_start and npoints_end
    j = 0
    jj = 0
    k = time_synthetic(1)
    kk = time_synthetic(1)
    do i = 1, synthetic_raw%npoints(irecord)
      if (k .le. end_min) then
        j = j + 1
        k = time_synthetic(1) + synthetic_raw%sample_rate(irecord)*dble(j-1)
      endif
      if (kk .le. beg_max) then
        jj = jj + 1
        kk = time_synthetic(1) + synthetic_raw%sample_rate(irecord)*dble(jj-1)
      endif
    enddo
    npoints_start = jj -1
    npoints_end = j

    call cut_define_check(beg_max, end_min, npoints_cut_synthetic, cuterr,&
        npoints_start, npoints_end, nfillb, nfille, nerr)
    cut_length = npoints_end - npoints_start + 1
    allocate(cut_synthetic(cut_length))
    call cut(sngl(synthetic_raw%records(irecord)%record), npoints_start, npoints_end,&
              nfillb, nfille, cut_synthetic)
    npoints_synthetic = size(cut_synthetic)
    call wsac1("cut.obs", cut_observed, npoints_observed,&
             beg_max, sngl(observed_raw%sample_rate(irecord)), nerr)
    call wsac1("cut.syn", cut_synthetic, npoints_synthetic,&
             beg_max, sngl(synthetic_raw%sample_rate(irecord)),nerr)
    print *, "first cut done"

!-----------------------------------------------------------------------------------
! PREPARE FOR DECONVOLUTION: rmean/rtrend/taper
!-----------------------------------------------------------------------------------

    call rmean(cut_observed, npoints_observed, mean)
    call rtrend(cut_observed, npoints_observed, yint, slope, &
                beg_max, sngl(observed_raw%sample_rate(irecord)))
    call taper_width_to_points(.05, npoints_synthetic, ipts)
    call taper(cut_observed, npoints_observed, 2, ipts)

    call rmean(cut_synthetic, npoints_synthetic, mean)
    call rtrend(cut_synthetic, npoints_synthetic, yint, slope, &
                beg_max, sngl(synthetic_raw%sample_rate(irecord)))
    call taper_width_to_points(.05, npoints_synthetic, ipts)
    call taper(cut_synthetic, npoints_synthetic, 2, ipts)
    print *, "prepared for deconvolution"

!-----------------------------------------------------------------------------------
! REMOVE INSTRUMENT RESPONSE
!-----------------------------------------------------------------------------------

    call remove_response(cut_observed, cut_synthetic, npoints_observed, npoints_synthetic,&
                    synthetic_raw%sample_rate(irecord), observed_raw, irecord)
    print *, "removed response"

!-----------------------------------------------------------------------------------
! rmean/rtrend/taper again in case instrument response introduces artifacts
!-----------------------------------------------------------------------------------

    call rmean(cut_observed, npoints_observed, mean)
    call rtrend(cut_observed, npoints_observed, yint, slope, &
                beg_max, sngl(observed_raw%sample_rate(irecord)))
    call taper_width_to_points(.05, npoints_synthetic, ipts)
    call taper(cut_observed, npoints_observed, 2, ipts)

    call rmean(cut_synthetic, npoints_synthetic, mean)
    call rtrend(cut_synthetic, npoints_synthetic, yint, slope, &
                beg_max, sngl(synthetic_raw%sample_rate(irecord)))
    call taper_width_to_points(.05, npoints_synthetic, ipts)
    call taper(cut_synthetic, npoints_synthetic, 2, ipts)
    print *, "prepared for deconvolution"

!-----------------------------------------------------------------------------------
! RESAMPLE OBSERVED AND SYNTHETIC SEISMOGRAMS TO SAME SAMPLE RATE
!-----------------------------------------------------------------------------------

    call interp(cut_synthetic, npoints_synthetic, cut_synthetic, npoints_synthetic,&
                beg_max, end_min,&
                sngl(synthetic_raw%sample_rate(irecord)), beg_max, &
                sngl(synthetic_raw%sample_rate(irecord)), error)
    call interp(cut_observed, npoints_observed, cut_observed, npoints_synthetic,&
                beg_max, end_min,&
                sngl(observed_raw%sample_rate(irecord)), beg_max,&
                sngl(synthetic_raw%sample_rate(irecord)), error)
    print *, "resample complete"
    !call wsac1("cut.obs", cut_observed, npoints_synthetic,&
    !      beg_max, sngl(synthetic_raw%sample_rate(irecord)), nerr)
    !call wsac1("cut.syn", cut_synthetic, npoints_synthetic,&
    !         beg, sngl(synthetic_raw%sample_rate(irecord)), nerr)

!-----------------------------------------------------------------------------------
! FINAL CUT OF OBSERVED AND SYNTHETIC SEISMOGRAMS
!-----------------------------------------------------------------------------------

    ! define larger of observed and synthetic begin time
    beg_max=sngl(time_synthetic(1))+1
    ! define smaller of observed and synthetic end time
    end_min=min(sngl(time_observed(npoints_observed)),sngl(time_synthetic(npoints_synthetic)))-1

    ! Define cut for observed trace
    call cut_define(beg_max, sngl(synthetic_raw%sample_rate(irecord)), end_min, npoints_cut_observed)

    ! Define npoints_start and npoints_end
    j = 0
    jj = 0
    k = time_synthetic(1)
    kk = time_synthetic(1)
    do i = 1, npoints_synthetic
      if (k .le. end_min) then
        j = j + 1
        k = time_synthetic(1) + synthetic_raw%sample_rate(irecord)*dble(j-1)
      endif
      if (kk .le. beg_max) then
        jj = jj + 1
        kk = time_synthetic(1) + synthetic_raw%sample_rate(irecord)*dble(jj-1)
      endif
    enddo
    npoints_start = jj -1
    npoints_end = j
    call cut_define_check(beg_max, end_min, npoints_cut_observed, cuterr,&
        npoints_start, npoints_end, nfillb, nfille, nerr)
    cut_length = npoints_end - npoints_start + 1
    allocate(observed_final(cut_length))
    ! Cut observed trace
    call cut(cut_observed, npoints_start, npoints_end,&
              nfillb, nfille, observed_final)
    npoints_observed = size(observed_final)

    ! Define cut for synthetic trace
    call cut_define(beg_max, sngl(synthetic_raw%sample_rate(irecord)), end_min, npoints_cut_synthetic)

    call cut_define_check(beg_max, end_min, npoints_cut_synthetic, cuterr,&
        npoints_start, npoints_end, nfillb, nfille, nerr)
    cut_length = npoints_end - npoints_start + 1
    allocate(synthetic_final(cut_length))
    ! Cut synthetic trace
    call cut(cut_synthetic, npoints_start, npoints_end,&
              nfillb, nfille, synthetic_final)
    npoints_synthetic = size(synthetic_final)

    call wsac1("cut.obs", cut_observed, npoints_observed,&
          sngl(time_synthetic(1)), sngl(synthetic_raw%sample_rate(irecord)), nerr)
    call wsac1("cut.syn", cut_synthetic, npoints_synthetic,&
             sngl(time_synthetic(1)), sngl(synthetic_raw%sample_rate(irecord)), nerr)
    print *, "final cut finished"

!-----------------------------------------------------------------------------------
! COPY PROCESSED DATA TO ASDF CONTAINER
!-----------------------------------------------------------------------------------

  endif
  call store_asdf_data(observed_raw, observed_proc, synthetic_raw, synthetic_proc, dble(beg_max), irecord)
  enddo
  call rotate_traces(observed_proc, observed_rotate)
  call rotate_traces(synthetic_proc, synthetic_rotate)
  print *, "rotation finished"

  call deallocate_all(observed_raw, synthetic_raw)

end subroutine process_asdf
end module
