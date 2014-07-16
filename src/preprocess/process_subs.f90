! Authors James Smith; Ebru Bozdag
! Authors Wenjie Lei(07/2014)

module process_subs

  use asdf_data
  use mpi
  !use process_par
  !use process_var

  implicit none

! TOLERANCE CONTROL
  double precision, parameter ::  TOL=1e-7

contains

!=======================================================================
!> Read the contents of the CMT solutions file
!! \param CMT_FILE The location of the CMT file
subroutine read_CMT(event_name, rank, comm, ierr)

  use process_var
  implicit none

  integer :: rank, comm, ierr
  character(len=13) :: event_name

  character(len=5) datasource
  character(len=20) e_n
  character(len=150) string
  double precision :: elat_pde,elon_pde,depth_pde
  real :: mb, ms

  if (rank.eq.0) then
    CMT_FILE = 'CMT/CMT_'//trim(event_name)
    inquire(file=CMT_FILE,exist=file_exists)
    if (file_exists .eq. .false.) then
      stop 'error opening CMTSOLUTION file'
    endif
    open(1,file=CMT_FILE,status='old',action='read')
    read(1,*)datasource,yr,mo,da,ho,mi,sec,elat_pde,elon_pde,depth_pde,mb,ms
    ! ignore line with event name
    read(1,"(a)") string
    read(string(12:len_trim(string)),*) e_n

    ! read time shift
    read(1,"(a)") string
    read(string(12:len_trim(string)),*) delta_t

    ! read half duration
    read(1,"(a)") string
    read(string(15:len_trim(string)),*) cmt_hdur

    ! read latitude
    read(1,"(a)") string
    read(string(10:len_trim(string)),*) cmt_lat

    ! read longitude
    read(1,"(a)") string
    read(string(11:len_trim(string)),*) cmt_lon

    ! read depth
    read(1,"(a)") string
    read(string(7:len_trim(string)),*) cmt_depth

    ! ignore the last 6 lines with moment tensor info
    read(1,"(a)") string
    read(1,"(a)") string
    read(1,"(a)") string
    read(1,"(a)") string
    read(1,"(a)") string
    read(1,"(a)") string
    close(1)

  endif

end subroutine read_CMT

!======================================================================
!> Computes Julian Day and origin of event
subroutine event_origin(rank, comm, ierr)
!-------------------------------------------------------------------------

  use process_var
  implicit none
  integer :: jda, time_sec
  integer :: rank, comm, ierr

  if (rank .eq. 0) then
    jda = julian_day(yr,mo,da)
    gmt_year=yr
    gmt_day=jda
    gmt_hour=ho
    gmt_min=mi
    gmt_sec=int(sec+delta_t)
    gmt_msec=int((sec+delta_t-int(sec+delta_t))*1000)
  ! Adjust event time and date after t_shift is added
    if (gmt_sec >= 60) then
      time_sec = jda*24*3600 + ho*3600 + mi*60 + int(sec+delta_t)
      gmt_day  = int(time_sec/(24*3600))
      gmt_hour = int(mod(time_sec,24*3600)/3600)
      gmt_min  = int(mod(time_sec,3600)/60)
      gmt_sec  = mod(time_sec,60)
      if (gmt_day > 365 .and. .not. is_leap_year(gmt_year)) then
        gmt_day = mod(gmt_day,365)
        gmt_year = yr + 1
       elseif (gmt_day > 366 .and. is_leap_year(gmt_year)) then
        gmt_day= mod(gmt_day,366)
        gmt_year= yr + 1
       elseif (gmt_day == 366 .and. is_leap_year(gmt_year)) then
         gmt_day = 366
      endif
    endif
  endif
  call MPI_Bcast(gmt_year,1,MPI_INTEGER,0,comm,ierr)
  call MPI_Bcast(gmt_day,1,MPI_INTEGER,0,comm,ierr)
  call MPI_Bcast(gmt_hour,1,MPI_INTEGER,0,comm,ierr)
  call MPI_Bcast(gmt_min,1,MPI_INTEGER,0,comm,ierr)
  call MPI_Bcast(gmt_sec,1,MPI_INTEGER,0,comm,ierr)
  call MPI_Bcast(gmt_msec,1,MPI_INTEGER,0,comm,ierr)

end subroutine event_origin
 
!======================================================================
!> Adjusts begin time of observed data to CMT solution 
! Zero time in the synthetic seismograms corresponds to the centroid time (CMT
! time) of the earthquake. Normal-mode synthetic seismogram start at time t = 0,
! but, for numerical reasons, spectral-element seismograms start at negative times
! (t < 0).
!! \param begin_time The beginning time of the observed data
subroutine adjust_event_time(observed, ierr)

  use process_var
  type (asdf_event),intent(inout) :: observed
  integer :: ierr

  integer :: i
  double precision :: t_shift

  !origin_time = dble(gmt_hour)*3600.0 + dble(gmt_min)*60.0 + dble(gmt_sec) + dble(gmt_msec)/1000.0
  do i=1, observed%nrecords
    !begin_time = observed%gmt_hour(irecord)*3600.0+observed%gmt_min(irecord)*60.0 &
    !     + observed%gmt_sec(irecord) + observed%gmt_msec(irecord)/1000.0

    !calculate the time shift
    !t_shift = begin_time-origin_time
    t_shift=(observed%gmt_day(i)-gmt_day)*3600.0*24.0+(observed%gmt_hour(i)-gmt_hour)*3600.0&
      +(observed%gmt_min(i)-gmt_min)*60&
      +(observed%gmt_sec(i)-gmt_sec)+(observed%gmt_msec(i)-gmt_msec)/1000.0&
      +(observed%gmt_year(i)-gmt_year)*365.0*3600.0*24.0

    print *, "origin time:", gmt_year, gmt_day, gmt_hour, gmt_min, gmt_sec, gmt_msec
    print *, "time:", observed%gmt_min(i), observed%gmt_sec(i), observed%gmt_msec(i)

    !adjust the time to origin time and begin value
    observed%begin_value(i) = observed%begin_value(i)+t_shift
    observed%gmt_year(i) = gmt_year
    observed%gmt_day(i) = gmt_day
    observed%gmt_hour(i) = gmt_hour
    observed%gmt_min(i) = gmt_min
    observed%gmt_sec(i) = gmt_sec
    observed%gmt_msec(i) = gmt_msec

    !write(*,*)"event origin time:            ", origin_time
    !write(*,*)"observed reference time (beginning time):", begin_time
    write(*,*)"time-shift to be applied to observed files:   ", t_shift

    if (gmt_year .ne. observed%gmt_year(i)) then
      write(*,*)"year CMT file:",gmt_year,", year asdf file:",observed%gmt_year(i)
      stop 'ERROR: event year from CMT and asdf file do not match!'
    endif

    !if (origin_time .lt. observed%begin_value(irecord)) then
    !  write(*,*)"event origin time (s):  ",origin_time
    !  write(*,*)"observed data beginning time (s):   ",begin_time
    !  stop 'ERROR: data starts after event origin time!'
    !endif
  enddo

end subroutine adjust_event_time

!=======================================================================
!> Cut
!! This subroutine cuts the observed data and synthetic data
!! Before cutting, make sure the data and synthetic has the same reference time
subroutine cut_seis(obsd, npts1, dt1, b1, synt, npts2, dt2, b2, &
    obsd_cut, npts1_cut, dt1_cut, b1_cut, synt_cut, npts2_cut, &
    dt2_cut, b2_cut)

    use process_var, only : MAX_TRACE_LENGTH
  
    !varialbe in parameter list
    real, dimension(*) :: obsd, synt
    integer :: npts1, npts2
    real ::  dt1, b1, dt2, b2

    real, dimension(*) :: obsd_cut, synt_cut
    integer :: npts1_cut, npts2_cut
    real :: dt1_cut, b1_cut, dt2_cut, b2_cut

    !local variable
    real, dimension(:), allocatable :: temp_array
    real(kind=4), dimension(:), allocatable :: time_observed, time_synthetic
    integer :: j, jj, k, kk

    integer :: npoints_cut_observed, npoints_cut_synthetic
    integer :: cuterr, nerr, nfillb, nfille
    integer :: npoints_start, npoints_end, cut_length

    real :: beg_max, end_min
    integer :: i

    ! generate time arrays
    allocate(time_observed(npts1))
    do i = 1, npts1
      time_observed(i) = b1 + dt1*(i-1)
    enddo
    allocate(time_synthetic(npts2))
    do i = 1, npts2
      time_synthetic(i) = b2 + dt2*(i-1)
    enddo

    print *,"here"

    ! define larger of observed and synthetic begin time
    beg_max=max(b1,b2)+1
    print *,"here, beg_max:", beg_max
    print *, "npts1, npts2:", npts1, npts2
    ! define smaller of observed and synthetic end time
    end_min=min(time_observed(npts1),time_synthetic(npts2))-1
    print *,"here"
    !++++++++++++++
    !cut observed
    ! Define cut for observed trace
    call cut_define(beg_max, dt1, end_min, npoints_cut_observed)
    print *,"here define"
    ! Define npoints_start and npoints_end
    j = 0
    jj = 0
    k = time_observed(1)
    kk = time_observed(1)
    do i = 1, npts1
      if (k .le. end_min) then
        j = j + 1
        k = time_observed(1) + dt1*(j-1)
      endif
      if (kk .le. beg_max) then
        jj = jj + 1
        kk = time_observed(1) + dt1*(jj-1)
      endif
    enddo
    npoints_start = jj -1
    npoints_end = j
    call cut_define_check(beg_max, end_min, npoints_cut_observed, cuterr,&
        npoints_start, npoints_end, nfillb, nfille, nerr)
    cut_length = npoints_end - npoints_start + 1
    allocate(temp_array(cut_length))

    ! Cut observed trace
    call cut(obsd, npoints_start, npoints_end,&
              nfillb, nfille, temp_array)
    npts1_cut = cut_length
    dt1_cut = dt1
    b1_cut = beg_max
    obsd_cut(1:npts1_cut) = temp_array(1:npts1_cut)
    deallocate(temp_array)

    !++++++++++++++
    !cut synthetic
    ! Define cut for synthetic trace
    call cut_define(beg_max, dt2, end_min, npoints_cut_synthetic)
    ! Define npoints_start and npoints_end
    j = 0
    jj = 0
    k = time_synthetic(1)
    kk = time_synthetic(1)
    do i = 1, npts2
      if (k .le. end_min) then
        j = j + 1
        k = time_synthetic(1) + dt2*dble(j-1)
      endif
      if (kk .le. beg_max) then
        jj = jj + 1
        kk = time_synthetic(1) + dt2*dble(jj-1)
      endif
    enddo
    npoints_start = jj -1
    npoints_end = j

    call cut_define_check(beg_max, end_min, npoints_cut_synthetic, cuterr,&
        npoints_start, npoints_end, nfillb, nfille, nerr)
    cut_length = npoints_end - npoints_start + 1
    allocate(temp_array(cut_length))

    call cut(synt, npoints_start, npoints_end,&
              nfillb, nfille, temp_array)
    npts2_cut = cut_length
    dt2_cut = dt2
    b2_cut = beg_max
    synt_cut(1:npts2_cut) = temp_array(1:npts2_cut)
    deallocate(temp_array)

end subroutine cut_seis

!=======================================================================
!> Fourier transform
!! This inputs and outputs a complex function.
!! The convention is FFT --> e^(-iwt)
!! numerical factor for Plancerel Theorem: planch_fac = dble(FFT_NPTS * dt * dt)
!! \param n The exponent of the array
!! \param xi The complex function
!! \param zzign The sign for the Fourier transform
!! \param dt The sample rate for the Fourier transform 
subroutine fft(n,xi,zzign,dt)
  complex, dimension(*) :: xi
  integer :: n
  real(kind=8) :: dt

  integer :: FFT_NPTS

  double precision, parameter :: PI = 3.141592653589793d+00
  complex :: wk, hold, q
  real :: m(25)
  real :: zzign,zign,flx,v
  integer :: lblock,k,fk,jh,ii,istart
  integer :: l,iblock,nblock,i,lbhalf,j,lx

  ! sign must be +1. or -1.
  if(zzign >= 0.) then
    zign = 1.
  else
    zign = -1.
  endif

  lx = 2**n

  ! checks bounds
  if( lx > FFT_NPTS ) stop 'error fft increase NPT, or decrease n'
  do 1 i=1,n
  1 m(i) = 2**(n-i)
    do 4 l=1,n
    nblock = 2**(l-1)
    lblock = lx/nblock
    lbhalf = lblock/2
    k = 0
    do 4 iblock=1,nblock
    fk = k
    flx = lx

    v = zign*2.*PI*fk/flx         ! Fourier convention

    wk = cmplx(cos(v),-sin(v))   ! sign change to -sin(v) 17-Nov-2006
    istart = lblock*(iblock-1)

    do 2 i=1,lbhalf
    j  = istart+i
    jh = j+lbhalf
    ! checks bounds
    if( jh < 1 .or. jh > FFT_NPTS ) stop 'error fft bounds'

    q = xi(jh)*wk
    xi(jh) = xi(j)-q
    xi(j)  = xi(j)+q
  2 continue

    do 3 i=2,n
    ii = i
    if(k < m(i)) go to 4
  3 k = k-m(i)
  4 k = k+m(ii)
    k = 0
    do 7 j=1,lx
    if(k < j) go to 5
    hold = xi(j)
    if( k+1 < 1 .or. k+1 > FFT_NPTS ) stop 'error fft k bounds'
    xi(j) = xi(k+1)
    xi(k+1) = hold
  5 do 6 i=1,n
    ii = i
    if(k < m(i)) go to 7
  6 k = k-m(i)
  7 k = k+m(ii)

    ! final steps deal with dt factors
    if(zign > 0.) then       ! FORWARD FFT
      do i = 1,lx
        xi(i) = xi(i)*dt   ! multiplication by dt
      enddo

    else                     ! REVERSE FFT
      flx = flx*dt
      do i = 1,lx
        xi(i) = xi(i)/flx  ! division by dt
      enddo
    endif

end subroutine fft

!=======================================================================
!> Inverse Fourier transform
!! Calls fft for Fourier transform
!! \param npow Exponent for Fourier transform
!! \param s Complex Function
!! \param zzign Sign for Fourier transform
!! \param dt Sample rate of complex function
!! \param r Real part of complex function
subroutine fftinv(npow,s,zzign,dt,r)

  complex, intent(in) :: s(*)
  real(kind=8), intent(out) :: r(*)   ! note this is REAL

  real :: zzign, zign
  real(kind=8) :: dt
  integer :: npow, nsmp, nhalf, i

  nsmp = 2**npow
  nhalf = nsmp/2
  call rspec(s,nhalf)   ! re-structuring

  zign=zzign
  call fft(npow,s,zign,dt)    ! Fourier transform

  do i = 1,nsmp
    r(i) = real(s(i))     ! REAL part
  enddo
end subroutine fftinv

!======================================================================
!> Re-structure Fourier transform
!! \param s Complex Function
!! \param np2 Exponent for Fourier transform
subroutine rspec(s,np2)

  complex :: s(*)
  integer :: np2,n,n1,i

  n = 2*np2
  n1 = np2+1

  s(n1) = 0.
  s(1)  = cmplx( real(s(1)),0.)

  do i = 1,np2
    s(np2+i) = conjg(s(np2+2-i))
  enddo
end subroutine rspec

!======================================================================
!> Remove the mean from a time-series
!! \param x The time-series
!! \param npts The number of points in the time-series
subroutine demean(x, npts, xmean)

  integer npts, i
  real(kind=4), dimension(*) :: x
  real(kind=4) xsum, xmean

  xmean=0.0
  if(npts.gt.1) then
    xsum = 0.00d+00
    do i = 1,npts
      xsum = xsum + x(i)
    enddo

    xmean = xSum / npts
    do i = 1, npts
      x(i) = x(i) - xmean
    enddo
  endif
  return

end subroutine demean
           
!======================================================================
!> Apply a taper to a time-series
!! \param x The time-series
!! \param n The number of points in the time-series
!! \param t_type The type of taper to apply
!! \param width The number of points corresponding to the width
subroutine t_taper(x, n, t_type, width)

  use process_par

  integer, intent(in) :: n, t_type
  real(kind=8), dimension(*), intent(inout) :: x
  real(kind=8), intent(in) :: width

  integer i, n_width
  double precision omega, f0, f1

  ! set the number of points corresponding to the taper width
  n_width=int(floor(n*width))

  ! set the taper properties according to type
  select case (t_type)
    case (HANNING)
      omega = PI/dble(n_width)
      f0 = 0.5
      f1 = 0.5
    case (HAMMING)
      omega = PI/dble(n_width)
      f0 = 0.54
      f1 = 0.46
    case (COSINE)
      omega = PI/(2*dble(n_width))
      f0 = 1.0
      f1 = 1.0
  end select

  ! apply the taper symmetrically to both ends of the data
  do i = 1, n_width
    x(i) = x(i) * (f0-f1*cos(omega*(i-1)))
    x(n+1-i) = x(n+1-i) * (f0-f1*cos(omega*(i-1)))
  end do
end subroutine t_taper

!======================================================================
!> Remove a trend from a time-series
!! \param x The time-series
!! \param n The number of points in the time-series
subroutine detrend(x,n)

  integer, intent(in) :: n
  real(kind=8), dimension(*) :: x

  double precision :: ds1,ds2,dan,davei,davex,dslope,dai
  integer :: i, an

  an = n
  dan=n
  ds1=0
  ds2=0

  do i=1,n
    ds1 = ds1+ x(i)
    ds2 = ds2 + ds1
  enddo
  davei = 0.5 * (dan+1.0)
  davex = ds1/dan
  dslope = -12.0*(ds2-davei*ds1)/(dan*(dan*dan-1.0))
  do i=1,n
    dai = i-1
    x(i) = x(i)- davex - dslope*(dai-davei)
  enddo
end subroutine detrend

!======================================================================
!> Rotates horizontal components to radial and transverse
!! \param observed_final The processed observed seismogram
!! \param synthetic_final The processed synthetichetic seismogram
!! \param npoints_synthetic The number of points in the seismogram
subroutine rotate_traces(my_asdf, my_asdf_rotate)

    use asdf_read_subs
    use process_par
    use process_var

    type(asdf_event) :: my_asdf, my_asdf_rotate

    integer :: i, j, k
    integer :: num_stations, sta_index

    logical :: z_exist, t_exist, r_exist
    integer :: loc_z, loc_r, loc_t, loc_e, loc_n
    integer :: nn, nn_e, nn_z, nn_n

    real ::  azm,bzm,ddg,dkm
    real ::  costh, sinth
    real(kind=8) :: cmt_lat2, stlat2, rlo, delta, azep, azst

    real, allocatable :: zdata(:), rdata(:), tdata(:), ndata(:), edata(:)

    character(len=32), allocatable :: sta_list(:), nw_list(:)
    character(len=32) :: sta, nw

    real :: rad, fl, ecc
    real :: tt, dtt
    real :: baz
    integer :: icomp

    allocate(sta_list(my_asdf%nrecords))
    allocate(nw_list(my_asdf%nrecords))

    sta_index=0
    !first see how many stations in the my_asdf. then allocate the rotated data
    !structure by 3*num_stations
    sta_list(1)=my_asdf%receiver_name_array(1)
    nw_list(1)=my_asdf%network_array(1)
    do i=1, my_asdf%nrecords
      sta=my_asdf%receiver_name_array(i)
      nw=my_asdf%network_array(i)
      if(sta_exist(sta, nw, sta_list, nw_list, sta_index))then
        cycle
      else
       sta_index=sta_index+1
       sta_list(i)=my_asdf%receiver_name_array(i)
       nw_list(i)=my_asdf%network_array(i)
      endif
    enddo
    num_stations=sta_index

    call init_asdf_data(my_asdf_rotate,3*num_stations, .false.)

    rad=PI/180.0
    fl=0.00335293
    ecc=(1.-fl)**2.
    cmt_lat2=atan(ecc*tan(cmt_lat*rad))*(180/PI)
   !recalculate the az and baz
    !****W: dont know if you need to do that, James
    do i=1, my_asdf%nrecords
    !  call distaz(my_asdf%event_lat(i), my_asdf%event_lo(i), &
    !                my_asdf%receiver_lat(i), my_asdf%receiver_lo(i),&
    !                azm, bzm, ddg, dkm)
      stlat2=atan(ecc*tan(my_asdf%receiver_lat(irecord)*rad))*(180/PI)
      call SPH_AZI(cmt_lat2,cmt_lon,stlat2,rlo,delta,azep,azst)
      my_asdf%ev_to_sta_AZ(i)=azst
      my_asdf%sta_to_ev_AZ(i)=azst+180
    enddo
    !loop over sta list 
    do i=1, num_stations

      !********W: modify here, with E and N *******************************
      call locate_record(sta_list(i), nw_list(i), "Z", my_asdf%receiver_name_array, &
        my_asdf%network_array, my_asdf%component_array, num_stations, loc_z)
      call locate_record(sta_list(i), nw_list(i), "E", my_asdf%receiver_name_array, &
        my_asdf%network_array, my_asdf%component_array, num_stations, loc_r)
      call locate_record(sta_list(i), nw_list(i), "N", my_asdf%receiver_name_array, &
        my_asdf%network_array, my_asdf%component_array, num_stations, loc_t)
     if(loc_r>0.and.loc_t>0)then
        nn_e=my_asdf%npoints(loc_r)
        nn_n=my_asdf%npoints(loc_t)
        if(nn_n.ne.nn_e)then
          print *, "npoints of e and w don't agree:", &
            trim(my_asdf%receiver_name_array(loc_r))
            nn = 1
        else
          nn=nn_e
        endif
      else
        print *,"Missing e or w"
        nn=1
      endif
      if(loc_z>0)then
        nn_z=my_asdf%npoints(loc_z)
        if(loc_r>0)then
          if(nn_z.ne.nn)then
            print *,"npoints of Z and e(w) doesn't agree!"
            nn = 1
          endif
        else
          nn=nn_z
        endif
      endif

        bzm=my_asdf%sta_to_ev_AZ(loc_r)
        tt=my_asdf%begin_value(loc_r)
        dtt=my_asdf%sample_rate(loc_r)
        bzm=my_asdf%sta_to_ev_AZ(loc_z)
        tt=my_asdf%begin_value(loc_z)
        dtt=my_asdf%sample_rate(loc_z)
    ! STORE as E,N,Z
      allocate(zdata(nn))
      allocate(rdata(nn))
      allocate(tdata(nn))
      allocate(edata(nn))
      allocate(ndata(nn))
      allocate(my_asdf_rotate%records(3*i-2)%record(nn))
      allocate(my_asdf_rotate%records(3*i-1)%record(nn))
      allocate(my_asdf_rotate%records(3*i)%record(nn))

      !rotate
      !if R and T exist
      if(loc_r>0.and.loc_t>0) then
        !rotates
        rdata(1:nn) = my_asdf%records(loc_r)%record(1:nn)
        tdata(1:nn) = my_asdf%records(loc_t)%record(1:nn)
        costh = cos(baz)
        sinth = sin(baz)
        edata(1:nn) = -costh * tdata(1:nn) - sinth * rdata(1:nn)
        ndata(1:nn) =  sinth * tdata(1:nn) - costh * rdata(1:nn)
        my_asdf_rotate%records(3*i-2)%record(1:nn)= edata(1:nn)
        my_asdf_rotate%records(3*i-1)%record(1:nn)= ndata(1:nn)
      else
        !just write Z and set RT to zero
        my_asdf_rotate%records(3*loc_z-2)%record(1:nn)= 0.0
        my_asdf_rotate%records(3*loc_z-1)%record(1:nn)= 0.0
      endif

      !if Z exists
      if(loc_z>0)then
        zdata(1:nn) = my_asdf%records(loc_z)%record(1:nn)
        my_asdf_rotate%records(3*i)%record(1:nn)  = zdata(1:nn)
      else
        my_asdf_rotate%records(3*i)%record(1:nn)  = 0.0
      endif
      !===================================================
      !fill other info
      my_asdf_rotate%component_array(3*i-2) = "LHR"
      my_asdf_rotate%component_array(3*i-1) = "LHT"
      my_asdf_rotate%component_array(3*i) = "LHZ"

      do icomp=1,3
        my_asdf_rotate%npoints(3*i-icomp) = nn
        my_asdf_rotate%sample_rate(3*i-icomp) = dtt
        my_asdf_rotate%begin_value(3*i-icomp) = tt
        my_asdf_rotate%receiver_name_array(3*i-icomp) = sta_list(i)
        my_asdf_rotate%network_array(3*i-icomp) = nw_list(i)
      enddo
      !===================================================
      deallocate(zdata)
      deallocate(rdata)
      deallocate(tdata)
      deallocate(edata)
      deallocate(ndata)

    enddo

  !component = synt_raw%component_array(irecord)(3:3) 
  !if ((component .eq. "E") .or. (component .eq. "1")) then
  !  print *, component
  !  print *, synt_raw%component_array(irecord+1)(3:3)
  !  if ((synt_raw%component_array(irecord+1)(3:3).ne."N")) then
  !   print *, "problem"
  !return
!endif
                 !(synt_raw%component_array(irecord+1)(3:3).ne."2")) return
    !allocate(east_observed(npoints_synthetic),stat=nerr)
    !allocate(east_synthetic(npoints_synthetic))
!east_observed(1:npoints_synthetic) = sngl(observed_final(1:npoints_synthetic))
    !east_synthetic(1:npoints_synthetic) = sngl(synthetic_final(1:npoints_synthetic))
  !elseif ((component .eq. "N") .or. (component .eq. "2")) then
    ! Check that east is allocated
    !if (.not. allocated(east_observed)) return

    ! Check that north and east components are the same length
   ! if (npoints_synthetic .ne. size(east_observed)) then
   !   print *, "north and east components have different length"
    !  deallocate(east_observed)
    !  deallocate(east_synthetic)
    !  return
   ! endif
   !1 allocate(north_observed(npoints_synthetic))
   ! allocate(north_synthetic(npoints_synthetic))
   ! north_observed = sngl(observed_final(1:npoints_synthetic))
   ! north_synthetic = sngl(synthetic_final(1:npoints_synthetic))
    ! Assumes east is read in before north
  !  allocate(radial_observed(npoints_synthetic))
   ! allocate(radial_synthetic(npoints_synthetic))
   ! allocate(transverse_observed(npoints_synthetic))
   ! allocate(transverse_synthetic(npoints_synthetic))

   !! call rotate(north_observed, east_observed, npoints_synthetic, azst, .true., .true.,&
   !     radial_observed, transverse_observed)
   ! call rotate(north_synthetic, east_synthetic, npoints_synthetic, azst, .true., .true.,&
   !     radial_synthetic, transverse_synthetic)

    !do i = 1, npoints_synthetic
    !  radial_observed(i) = east_observed(i) * sin((sngl(azst) + 180) * 2 * PI / 360)&
    !     + north_observed(i) * cos((sngl(azst) + 180) * 2 * PI / 360)
    !  transverse_observed(i) = east_observed(i) * cos((sngl(azst) + 180) * 2 * PI / 360)&
    !    - north_observed(i) * sin((sngl(azst) + 180) * 2 * PI / 360)
    !  radial_synthetic(i) = east_observed(i) * sin((sngl(azst) + 180) * 2 * PI / 360)& 
    !     + north_synthetic(i) * cos((sngl(azst) + 180) * 2 * PI / 360)
    !  transverse_synthetic(i) = east_observed(i) * cos((sngl(azst) + 180) * 2 * PI / 360)& 
    !     - north_synthetic(i) * sin((sngl(azst) + 180) * 2 * PI / 360)
    !enddo

   ! if (DEBUG) then
   !   call wsac1("radial.obs", radial_observed, npoints_synthetic,&
   !       sngl(time_observed(1:npoints_synthetic)), sample_rate, nerr)
   ! endif

    ! Deallocate the arrays used to store rotated components
    !deallocate(north_observed)
    !!deallocate(east_observed)
    !deallocate(north_synthetic)
    !deallocate(east_synthetic)
    !ROTATED = .TRUE.
    !obsd_proc%npoints(i) = npoints_synthetic  
    !synt_proc%npoints(i) = npoints_synthetic
    !allocate(obsd_proc%records(i)%record(npoints_synthetic))
    !allocate(synt_proc%records(i)%record(npoints_synthetic))
    !obsd_proc%records(i-1)%record(1:npoints_synthetic) = radial_observed(1:npoints_synthetic)
    !obsd_proc%component_array(i-1)(3:3) = "R"
    !obsd_proc%records(i)%record(1:npoints_synthetic) = transverse_observed(1:npoints_synthetic)
    !!!obsd_proc%component_array(i)(3:3) = "T"
    !synt_proc%records(i-1)%record(1:npoints_synthetic) = radial_synthetic(1:npoints_synthetic)
    !synt_proc%component_array(i-1)(3:3) = "R"
    !synt_proc%records(i)%record(1:npoints_synthetic) = transverse_synthetic(1:npoints_synthetic)
    !synt_proc%component_array(i)(3:3) = "T"
    !deallocate(radial_observed)
    !deallocate(transverse_observed)
    !deallocate(radial_synthetic)
    !deallocate(transverse_synthetic)
    !ROTATED = .FALSE.
  !endif

end subroutine rotate_traces

!======================================================================
!> Interpolates a seismogram to a given sample rate
!! \param syn The seismogram to interpolate
!! \param t1 The start time of the seismogram
!! \param dt1 The sample rate of the seismogram
!! \param npt1 The number of points sampled in the seismogram
!! \param t2 The start time of the interpolated seismogram
!! \param dt2 The number of sample rate of the interpolated seismoram
!! \param npt2 The number of points in the interpolated seismogram
!subroutine interpolate_seis(syn,t1,dt1,npt1,t2,dt2,npt2)

!  implicit none
!  real(kind=8), dimension(:),intent(inout) :: syn
!  integer,intent(in) :: npt1,npt2
!  integer, parameter :: NDIM = FFT_NPTS!260000
!  real(kind=8),intent(in) :: t1,dt1,t2,dt2

!  double precision :: syn1(NDIM), time, tt
!  integer i, ii
!  ! initializes trace holding interpolated values
!  syn1(1:npt2) = 0.
!  ! loops over number of time steps in complete trace
!  do i = 1, npt2

    ! sets time (in s) at this time step:
    ! t2 : start time of trace
    ! dt2: delta_t of a single time step
!    time = t2 + (i-1) * dt2

    ! checks if time is within measurement window
    ! t1: start time of measurement window
    ! npt1: number of time steps in measurement window
    ! dt1: delta_t of a single time step in measurement window
!    if (time > t1 .and. time < t1 + (npt1-1)*dt1) then
      ! sets index of time steps within this window: is 1 at the beginning of window
!      ii = floor((time-t1)/dt1) + 1

!      time increment within this single time step to match the exact value of time
!      tt = time - ((ii-1)*dt1 + t1)
      ! interpolates value of trace for the exact time
!      syn1(i) = (syn(ii+1)-syn(ii)) * tt/dt1 + syn(ii)
!    endif
!  enddo
  ! saves interpolated values to output trace
!  syn(1:npt2) = syn1(1:npt2)
  ! LQY: zero out any thing beyond npts
!  if (npt1 > npt2) syn(npt2+1:npt1)=0.

!  do i = 1, npoints_observed
    !time_observed_final(i) = time_long_observed(1)+dt2*(i-1)
!  enddo
!  do i = 1, npoints_synthetic
    !time_synthetic_final(i) = time_long_synthetic(1)+dt2*(i-1)
!  enddo
!end subroutine interpolate_seis

!======================================================================
!> Cuts the observed and synthetic seismograms
!! \param data The observed seismogram
!! \param synthetic The synthetichetic seismogram
!! \param t_d The time array for the observed seismogram
!! \param t_s The time array for the synthetichetic seismogram
!! \param npts_d The number of points in the observed seismogram
!! \param npts_s The number of points in the synthetichetic seismogram
!! \param beg_d The beginning value of the observed seismogram
!! \param beg_s The beginning value of the synthetichetic seismogram
!! \param data_cut The cut observed seismogram
!! \param synthetic_cut The cut synthetichetic seismogram
!! \param t_d_cut The time array for the cut observed seismogram
!! \param t_s_cut The time array for the cut synthetichetic seismogram
!! \param npts_d_start The beginning value of the cut observed seismogram
!! \param npts_d_end The end value of the cut observed seismogram
!! \param npts_s_start The beginning value of the cut synthetichetic seismogram
!! \param npts_s_end The end value of the cut synthetichetic seismogram
!subroutine cut_seismograms(observed, synthetic, begin_time_obsd, begin_time_synt, FIRST_CUT)

!  double precision, intent(inout) :: observed, synthetic
!  double precision, dimension(npts_d)    :: t_d, t_d_cut
!  double precision, dimension(npts_s)    :: t_s, t_s_cut
!  real, allocatable                 :: cut_d(:), cut_s(:)
!  integer                           :: npts_d, npts_s
!  integer                           :: cut_length
!  integer                           :: cuterr, n, nfillb, nfille, nerr, nstart, nstop
!  integer                           :: npts_d_start, npts_d_end
!  integer                           :: npts_s_start, npts_s_end
!  real                              :: beg_max, end_min
!  real(kind=8)                      :: dt_d, dt_s
!  double precision                  :: k, kk
!  integer                           :: i, j, jj 
!  logical                           :: FIRST_CUT
!  allocate(cut_d(npts_d))
!  allocate(cut_s(npts_s))
!  do i = 1, npts_d
!    t_d(i) = observed_raw%begin_time(irecord) + observed_raw%sample_rate(irecord)*dble(i-1)
!  enddo

!  do i = 1, npts_s
!    t_s(i) = synthetic_raw%begin_time(irecord) + synthetic_raw%sample_rate(irecord)*dble(i-1)
!  enddo

!  beg_max=max(sngl(observed_raw%begin_time(irecord)),sngl(synthetic_raw%begin_time(irecord))) + 1
!  end_min=min(sngl(t_d(observed_raw%npoints(irecord))),sngl(t_s(synthetic_raw%npoints(irecord)))) - 1

!  write(*,*)"beg_d, beg_s, beg_max: ",beg_d, beg_s, beg_max
!  write(*,*)"end_d, end_s, end_min: ",t_d(npts_d),t_s(npts_s), end_min

!  dt_d = t_d(3)-t_d(2)
!  dt_s = t_s(3)-t_s(2)
!  k = t_d(1)
!  kk = t_d(1)
!  j = 0 
!  jj = 0
!  do i = 1,npts_d
!    if (k .le. end_min) then
!      j = j + 1
!      k = beg_d + dt_d*dble(j-1)
!    endif
!    if (kk.le.beg_max) then
!      jj = jj + 1
!     kk =  beg_d + dt_d*dble(jj-1)
!    endif
!   enddo
!   npts_d_start = jj-1  
!   npts_d_end = j
!   k = t_s(1)
!   kk = t_s(1)
!   j = 0
!   jj = 0
!   do i = 1,npts_s
!     if (k .le. end_min) then
!       j = j + 1
!       k = beg_s + dt_s*dble(j-1)
!     endif
!     if (kk.le.beg_max) then
!       jj = jj + 1
!       kk =  beg_s + dt_s*dble(jj-1)
!     endif
!   enddo
!   npts_s_start = jj - 1 
!   npts_s_end = j
!  npoints_observed = (npts_d_end-npts_d_start)+1
!   npoints_synthetic = (npts_s_end-npts_s_start)+1
!   cuterr = 0
!   call cut_define(beg_max, sngl(dt_d), end_min, npoints_observed)
!   call cut_define_check(beg_max, end_min, npts_d, cuterr,&
!        npts_d_start, npts_d_end, nfillb, nfille, nerr)
!   cut_length = npts_d_end - npts_d_start + 1
!   call cut(data(1:npts_d), npts_d_start, npts_d_end, nfillb, nfille, cut_d(1:cut_length))
!   allocate(cut_observed(cut_length))
!   cut_observed(1:cut_length) = dble(cut_d(1:cut_length))

!   cuterr = 0
!   call cut_define(beg_max, sngl(dt_s), end_min, npoints_synthetic)
!   call cut_define_check(beg_max, end_min, npts_s, cuterr,&
!        npts_s_start, npts_s_end, nfillb, nfille, nerr)
!   cut_length = npts_s_end - npts_s_start + 1
!   call cut(sngl(synthetic(1:npts_s)), npts_s_start, npts_s_end, nfillb, nfille, cut_s(1:cut_length))
!   allocate(cut_synthetic(cut_length))
!   cut_synthetic(1:cut_length) = dble(cut_s(1:cut_length))

   !cut_s(1:npts_s) = 0.0
   !cut_d(1:npts_d) = 0.0
   !cut_s(1:npts_s_end-npts_s_start+1) = synthetic(npts_s_start:npts_s_end)
   !cut_d(1:npts_d_end-npts_d_start+1) = data(npts_d_start:npts_d_end)

!   do i= 1,npoints_synthetic
 !    t_s_cut(i) = t_s(npts_s_start) + dt_s * dble(i-1)
 !  enddo
!   do i= 1,npoints_observed
!     t_d_cut(i) = t_d(npts_d_start) + dt_d * dble(i-1)
!   enddo
!   if (FIRST_CUT) then
!     allocate(time_observed(npoints_observed))
!     allocate(time_synthetic(npoints_synthetic))
!   else
!     deallocate(time_observed)
!     deallocate(time_synthetic)
!     allocate(time_observed(npoints_observed))
!     allocate(time_synthetic(npoints_synthetic))
!   endif
!   time_observed(1:npoints_observed) = t_d_cut(1:npoints_observed)
!   time_synthetic(1:npoints_synthetic) = t_s_cut(1:npoints_synthetic) 
!   begin_observed = t_d_cut(1)
!   begin_synthetic = t_s_cut(1)
!end subroutine cut_seismograms

!======================================================================
!> Cuts one observed or synthetichetic seismograms
!! \param data The seismogram to cut
!! \param time The time array for the seismogram
!! \param npts The number of points in the seismogram
!! \param tmin The beginning value of the seismogram
!! \param tmax The end value of the seismogram
!! \param data_cut The cut seismogram
!! \param time_cut The time array for the cut seismogram
!! \param npts_start The beginning value of the cut seismogram
!! \param npts_end The end value of the cut seismogram
subroutine cut_one_seis(data,time,npts,tmin,tmax,data_cut,time_cut,npts_start,npts_end, start_time, sample_rate) 

  real, dimension(*)                :: time_cut
  real, dimension(*)                :: data
  real,dimension(npts)               :: data_cut, time
  integer                           :: npts, npts_cut
  integer                           :: npts_start, npts_end
  real                              :: tmin, tmax, beg
  real                              :: k, kk, dt
  real,intent(in)                   :: sample_rate, start_time
  integer                           :: i, j, jj

!  call safe_alloc(time, npts, "Error allocating time array")

  do i = 1, npts
    ! pass in start_time = observed%begin_value(icmp)+t_shift
    time(i) = start_time + sample_rate*dble(i-1)
  enddo

!  call safe_alloc(data_cut, npts)

  dt = time(3)-time(2)
    
  beg = time(1)
  k = beg
  kk = beg
 
  j = 0
  jj = 0
  do i = 1,npts
    if (k .le. tmax) then
      j = j + 1
      k = beg + dt*dble(j-1)
    endif
    if (kk.le.tmin) then
      jj = jj + 1
      kk =  beg + dt*dble(jj-1)
    endif
  enddo
   
  npts_start = jj-1
  npts_end = j 
  data_cut(1:npts) = 0.0
  data_cut(1:npts_end-npts_start+1) = data(npts_start:npts_end)

  do i= 1,npts_end-npts_start+1
    time_cut(i) = time(npts_start) + dt * sngl(i-1)
  enddo

end subroutine cut_one_seis

!======================================================================
!> Returns the power of 2 for a given number of points 
!! \param npts The number of points for determining npts=2**m
 subroutine npow2(npts)
!  Given npts, determine the N=2**m such that N >= npts
!  return the new ntps
!  from Robert Herrmann's seismic package
  integer*4 nsamp, npts
  nsamp = npts
  npts = 1
  1000  continue
  npts = 2*npts
  if(npts.lt.nsamp)goto 1000
  return
end subroutine npow2

!======================================================================
!> Computes the distance and azimuth between two points on a sphere
!! Notes:
!! 
!! (1) applies to geocentric not geographic lat,lon on Earth
!!  
!! (2) This routine is inaccurate for del less than about 0.5 degrees. 
!!     For greater accuracy, use double precision or perform a separate
!!     calculation for close ranges using Cartesian geometry.
!!
!! \param flat1 The latitude of the first point (degrees)
!! \param flon2 The longitude of the first point (degrees)
!! \param flat2 The latitude of the second point (degrees)
!! \param flon2 The longitude of the second point (degrees)
!! \param del The angular separation between points (degrees)
!! \param azi The azimuth at 1st point to 2nd point, from N (degrees)
subroutine SPH_AZI(flat1,flon1,flat2,flon2,del,azi,bazi)

  implicit none
  real(kind=8)         :: flon2
  real(kind=8) :: flat1,flon1,flat2,del,azi,pi,raddeg,theta1,theta2,  &
                      phi1,phi2,stheta1,stheta2,ctheta1,ctheta2,                &
                      sang,cang,ang,caz,saz,az, bazi
  real(kind=8) :: cbang,bang,sbang,sbaz,cbaz,baz
  if ( (flat1 == flat2 .and. flon1 == flon2) .or.     &
       (flat1 ==  90.  .and. flat2 ==  90.)  .or.     &
       (flat1 == -90.  .and. flat2 == -90.) )  then
     del=0.
     azi=0.
     return
  end if
  pi=3.1415926535897
  raddeg=pi/180.
  theta1=(90.-flat1)*raddeg
  theta2=(90.-flat2)*raddeg
  phi1=flon1*raddeg
  phi2=flon2*raddeg
  stheta1=sin(theta1)
  stheta2=sin(theta2)
  ctheta1=cos(theta1)
  ctheta2=cos(theta2)
  cang=stheta1*stheta2*cos(phi2-phi1)+ctheta1*ctheta2

  ang=acos(cang)
  del=ang/raddeg
  sang=sqrt(1.-cang*cang)
  caz=(ctheta2-ctheta1*cang)/(sang*stheta1)
  saz=-stheta2*sin(phi1-phi2)/sang
  az=atan2(saz,caz)
  azi=az/raddeg
  if (azi.lt.0.) azi=azi+360.

  cbang=stheta2*stheta1*cos(phi1-phi2)+ctheta2*ctheta1
  bang=acos(cbang)
  sbang=sqrt(1.-cbang*cbang)
  cbaz=(ctheta1-ctheta2*cbang)/(sbang*stheta2)
  sbaz=-stheta1*sin(phi2-phi1)/sbang
  baz=atan2(sbaz,cbaz)
  bazi=baz/raddeg
  if (bazi.lt.0.) bazi=bazi+360.

end subroutine SPH_AZI

!======================================================================
!> Computes the distance and back azimuth between two points on a sphere
!! Notes:
!! 
!! (1) applies to geocentric not geographic lat,lon on Earth
!!  
!! (2) This routine is inaccurate for del less than about 0.5 degrees. 
!!     For greater accuracy, use double precision or perform a separate
!!     calulation for close ranges using Cartesian geometry.
!!
!! \param flat1 The latitude of the first point (degrees)
!! \param flon2 The longitude of the first point (degrees)
!! \param flat2 The latitude of the second point (degrees)
!! \param flon2 The longitude of the second point (degrees)
!! \param del The angular separation between points (degrees)
!! \param azi The back azimuth at 2nd point to 1st point, from N (degrees)
subroutine SPH_BAZI(flat1,flon1,flat2,flon2,del,azi)

  implicit none
  real :: flat1,flon1,flat2,flon2,del,azi,pi,raddeg,theta1,theta2,  &
          phi1,phi2,stheta1,stheta2,ctheta1,ctheta2,                &
          sang,cang,ang,caz,saz,az
  if ( (flat1 == flat2 .and. flon1 == flon2) .or.     &
       (flat1 ==  90.  .and. flat2 ==  90.)  .or.     &
       (flat1 == -90.  .and. flat2 == -90.) )  then
     del=0.
     azi=0.
     return
  end if
  pi=3.141592654
  raddeg=pi/180.
  theta1=(90.-flat1)*raddeg
  theta2=(90.-flat2)*raddeg
  phi1=flon1*raddeg
  phi2=flon2*raddeg
  stheta1=sin(theta1)
  stheta2=sin(theta2)
  ctheta1=cos(theta1)
  ctheta2=cos(theta2)
  cang=stheta1*stheta2*cos(phi2-phi1)+ctheta1*ctheta2
  ang=acos(cang)
  del=ang/raddeg
  sang=sqrt(1.-cang*cang)
  caz=(ctheta2-ctheta1*cang)/(sang*stheta1)
  saz=-stheta2*sin(phi1-phi2)/sang
  az=atan2(saz,caz)
  azi=az/raddeg
  if (azi.lt.0.) azi=azi+360.
end subroutine SPH_BAZI

!======================================================================
!> Reads the SAC poles and zeros from SAC_PZs file
!! \param filename The name of the SAC_PZs file
!! \param npoles The number of poles
!! \param nzeros The number of zeros
!! \param p The complex poles
!! \param z The complex zeros
!! \param k The constant
subroutine read_pzs(filename,npoles,nzeros,p,z,k)

  implicit none
  character(len=80)              :: filename
  character(len=20)               :: dum, dum2
  integer                         :: npoles, nzeros
  double precision                :: k
  double precision, dimension(50) :: p_re,p_im, z_re,z_im
  complex*16, dimension(50)          :: p,z
  integer                         :: i_z, i,i_p,i_c,ios

  open(1,file=filename,iostat=ios,status='old',action='read')
  if(ios /= 0) stop 'error opening POLE-ZERO file'
  i = 0
  do while (dum .ne. "ZEROS")
    read(1,*)dum,dum2
    i = i+1
  enddo
  i_z = i
  do while (dum .ne. "POLES")
    read(1,*)dum,dum2
    i = i+1
  enddo
  i_p = i
  do while (dum .ne. "CONSTANT")
    read(1,*)dum,dum2
    i = i+1
  enddo
  i_c = i
  close(1)

  open(1,file=filename,iostat=ios,status='old',action='read')
  
  do while (dum .ne. "ZEROS")
    read(1,*)dum,dum2
  enddo
  read(1,*)dum,nzeros 

  do i=i_z, i_p-1
    read(1,*)z_re(i-1), z_im(i-1)
  enddo

  if (i_p < nzeros) then
    z_re(i_p:nzeros) = 0.0
    z_im(i_p:nzeros) = 0.0
  endif

  read(1,*)dum, npoles

  do i = i_p, (i_c-i_p)-1
    read(1,*)p_re(i), p_im(i)
  enddo

  if ((i_c-i_p)-1 < npoles) then
    p_re(i_c-i_p:npoles) = 0.0
    p_im(i_c-i_p:npoles) = 0.0
  endif

  read(1,*)dum,k
  p(1:npoles) = cmplx(p_re(1:npoles),p_im(1:npoles))
  z(1:nzeros) = cmplx(z_re(1:nzeros),z_im(1:nzeros))
end subroutine

!======================================================================
!> Reads the SAC poles and zeros from SAC_PZs file
!!    Notes:
!!     - Poles and Zeros must be in units of angular frequency.  So a pole
!!       at 2Hz on the real axis will be 2*pi*2Hz = 12.566+0i.  ZPK2CMPLX
!!       checks that all complex poles and zeros have a conjugate pair.
!!     - The formula for getting the complex spectra is:
!!
!!                 (iW-Z )*(iW-Z )*...*(iW-Z  )
!!                      1       2           NZ
!!        H(W)=K * ____________________________
!!                 (iW-P )*(iW-P )*...*(iW-P  )
!!                      1       2           NP
!!
!!       Where H(W) holds the complex spectra, W represents angular
!!       frequencies corresponding to 2*PI*F, and i is the imaginary number.
!!	
!!	wpow = 0 for displacement
!!	wpow = 1 for velocity
!! \param h The complex spectra
!! \param f The file
!! \param z The complex zeros
!! \param p The complex poles
!! \param k The constant
!! \param np The number of poles
!! \param nz The number of zeros
!! \param wpow Flag for the units
!subroutine zpk2cmplx(h,f,z,p,k,np,nz,wpow)
!
!  implicit none
!  complex*16, dimension(np)                :: p
!  complex*16, dimension(nz)                :: z
!  double precision, dimension(nz)          :: z_ones
!  double precision, dimension(np)          :: p_ones
!  double precision                         :: k
!  integer                                  :: wpow,np,nz,i
!  double precision, dimension(FFT_NPTS)    :: f
!  complex*16, dimension(FFT_NPTS)          :: h,w
!  complex*16                               :: hh
!  complex*16                               :: nm,dnm
!
!  w(1:FFT_NPTS) = cmplx(0.0,2*PI*f(1:FFT_NPTS))
!  z_ones(1:nz) = 1.0
!  p_ones(1:np) = 1.0
!  do i = 1, FFT_NPTS
!    nm = product(w(i)-z(1:nz))
!    dnm = product(w(i)-p(1:np))
!    hh = k*(nm/dnm)
!    h(i) = hh/w(i)**wpow
!  enddo
!end subroutine

!======================================================================
!> Stores the processed data into asdf data structure
!! \param raw The raw asdf data
!! \param proc The processed asdf data
!! \param i The record number
subroutine store_asdf_metadata(asdf_old, asdf_new)

  use asdf_read_subs ! used for init_asdf_data
  use var_main
  implicit none
  type (asdf_event),intent(inout) :: asdf_old, asdf_new

  integer :: i 
  real(kind=8) :: phi
  integer :: ierr
  
  print *, "initalizing asdf"
  asdf_new%min_period = MIN_PERIOD
  asdf_new%max_period = MAX_PERIOD
  asdf_new%event = asdf_old%event
  asdf_new%nreceivers = asdf_old%nreceivers

  print *, "initalized asdf"
  do i=1, asdf_old%nrecords 
    asdf_new%event_lat(i)=asdf_old%event_lat(i)
    asdf_new%event_lo(i)=asdf_old%event_lo(i)
    asdf_new%event_dpt(i)=asdf_old%event_dpt(i)

    asdf_new%gmt_year(i)=asdf_old%gmt_year(i)
    asdf_new%gmt_day(i)=asdf_old%gmt_day(i)
    asdf_new%gmt_hour(i)=asdf_old%gmt_hour(i)
    asdf_new%gmt_min(i)=asdf_old%gmt_min(i)
    asdf_new%gmt_sec(i)=asdf_old%gmt_sec(i)
    asdf_new%gmt_msec(i)=asdf_old%gmt_msec(i)

    asdf_new%receiver_lat(i)=asdf_old%receiver_lat(i)
    asdf_new%receiver_lo(i)=asdf_old%receiver_lo(i)
    asdf_new%receiver_el(i)=asdf_old%receiver_el(i)
    asdf_new%receiver_dpt(i)=asdf_old%receiver_dpt(i)

    !asdf_new%(i)=asdf_old%(i)
    !asdf_new%(i)=asdf_old%(i)

    asdf_new%receiver_name_array(i) = asdf_old%receiver_name_array(i)
    asdf_new%network_array(i) = asdf_old%network_array(i)
    asdf_new%component_array(i) = asdf_old%component_array(i)
    asdf_new%receiver_id_array(i) = asdf_new%receiver_id_array(i)

    !dummy
    asdf_new%end_value(i) = 0.0

    ! WJL: need to be checked in the future
    ! instrument orientation
    !if(modulo(i,3).eq.1) then !
    !  phi = azst
    !  if (phi>180.d0) then
    !    phi = phi-180.d0
    !  else if (phi<180.d0) then
    !    phi = phi+180.d0
    !  else if (phi==180.d0) then
    !    phi = azst
    !  endif
    !  obsd_proc%cmp_azimuth(i) = dble(modulo(phi,360.0))
    !  obsd_proc%cmp_incident_ang(i) = 90.00
    !  synt_proc%cmp_azimuth(i) = dble(modulo(phi,360.0))
    !  synt_proc%cmp_incident_ang(i) = 90.00
    !else if(modulo(i,3).eq.2) then !
    !  phi = azst
    !  if (phi>180.d0) then
    !    phi = phi-180.d0
    !  else if (phi<180.d0) then
    !    phi = phi+180.d0
    !  else if (phi==180.d0) then
    !    phi = azst
    !  endif
    !  obsd_proc%cmp_azimuth(i) = dble(modulo(phi+90.0,360.0))
    !  obsd_proc%cmp_incident_ang(i) = 90.00
    !  synt_proc%cmp_azimuth(i) = dble(modulo(phi+90.0,360.0))
    !  synt_proc%cmp_incident_ang(i) = 90.00
    !else if(modulo(i,3).eq.3) then !
    !  obsd_proc%cmp_azimuth(i)  = 0.00
    !  obsd_proc%cmp_incident_ang(i) = 0.00
    !  synt_proc%cmp_azimuth(i) = dble(modulo(phi+90.0,360.0))
    !  synt_proc%cmp_incident_ang(i) = 90.00
    !endif
    asdf_new%cmp_azimuth(i)=asdf_old%cmp_azimuth(i)
    asdf_new%cmp_incident_ang(i)=asdf_old%cmp_incident_ang(i)

    asdf_new%scale_factor(i) = 1
    asdf_new%ev_to_sta_AZ(i) = 0.0
    asdf_new%sta_to_ev_AZ(i) = 0.0
    asdf_new%great_circle_arc(i) = 0.0
    asdf_new%dist(i) = 0.0

    asdf_new%P_pick(i) = -12345
    asdf_new%S_pick(i) = -12345

    asdf_new%scale_factor(i) = 1
    asdf_new%ev_to_sta_AZ(i) = 0.0
    asdf_new%sta_to_ev_AZ(i) = 0.0
    asdf_new%great_circle_arc(i) = 0.0
    asdf_new%dist(i) = 0.0

  enddo

end subroutine store_asdf_metadata

subroutine ttimes(dist_deg,depth,nphases,names,times)

  implicit none
  integer, parameter :: MAX_PHASES=60

  real, intent(in) :: dist_deg, depth

  integer, intent(out) :: nphases
  character*8, dimension(*), intent(out) :: names
  double precision, dimension(*), intent(out) :: times

  ! legacy variables needed to use the libtau routines
  logical prnt(3)
  character*8 phlst(10)
  real usrc(2)
  real, dimension(MAX_PHASES) :: dtdd,dtdh,dddp
  real, dimension(MAX_PHASES) :: times_sngl
  character*262 modnam
  character*256 iaspmod

  phlst(1)="all"
  prnt(1)=.false.
  prnt(2)=.false.

  prnt(3)=.false.
  call getenv('IASPMODEL', iaspmod)
  if (trim(iaspmod) == '') then
    modnam='iasp91'
  else
    modnam=iaspmod
  endif
  call tabin(1,modnam)
  call brnset(1,phlst,prnt)
  call depset(depth,usrc)

  call trtm(dist_deg,MAX_PHASES,nphases,times_sngl,dtdd,dtdh,dddp,names)
  times(1:nphases) = dble(times_sngl(1:nphases))

  write(*,*) 'Found ', nphases, ' phases:'

end subroutine ttimes

subroutine print_help()
    print '(a)', 'usage: cmdline [OPTIONS]'
    print '(a)', ''
    print '(a)', 'Without further options, cmdline prints the date and exits.'
    print '(a)', ''
    print '(a)', 'cmdline options:'
    print '(a)', ''
    print '(a)', '  -v, --version     print version information and exit'
    print '(a)', '  -h, --help        print usage information and exit'
end subroutine print_help

!======================================================================
!> Used to determine if yr is a leap year
!! \param yr The year of the event
  integer function lpyr(yr)

    implicit none

    integer yr
!
!---- returns 1 if leap year
!
  lpyr=0
  if(mod(yr,400) == 0) then
    lpyr=1
  else if(mod(yr,4) == 0) then
    lpyr=1
    if(mod(yr,100) == 0) lpyr=0
  endif

  end function lpyr

!======================================================================
!> Used to determine the julian day
!! \param yr The year of the event
!! \param mo The month of the event
!! \param da The day of the event
  integer function julian_day(yr,mo,da)

  implicit none

  integer yr,mo,da

  integer mon(12)
  data mon /0,31,59,90,120,151,181,212,243,273,304,334/

  julian_day = da + mon(mo)
  if(mo>2) julian_day = julian_day + lpyr(yr)

  end function julian_day

! function to determine if year is a leap year
  logical function is_leap_year(yr)

  implicit none

  integer yr

  !integer, external :: lpyr


!---- function lpyr above returns 1 if leap year
  if(lpyr(yr) == 1) then
    is_leap_year = .true.
  else
    is_leap_year = .false.
  endif

  end function is_leap_year

!======================================================================
!> Applies a cubic taper to a time series
!! \param xl The lower bound
!! \param xh The upper bound
!! \param x The time series
real function taper3(xl,xh,x)
! from Robert Herrmann's seismic package
!------------------------------------------------------------------------
  real xl, xh, x
  real p
!------------------------------------------------------------------------
!  cubic taper between 0 and 1 for xl <= x <= xh,
!  with = 1 for x = xh and 0 for x = xl,
!------------------------------------------------------------------------
  if(xl .eq. xh)then
    taper3 = 1.0
  else
    p = (x - xl)/(xh - xl)
    p = 2.0*p - 1.0
    if(p .le. -1.0)then
  taper3 = 0.0
    else if(p .ge. 1.0)then
  taper3 = 1.0
    else
  taper3 = 0.5 + 0.75*p*(1.0 - p*p/3.0)
    endif
  endif
  return
end function taper3

!======================================================================
!> Converts an integer to a string
!! \param k The integer to convert
character(len=20) function str(k)
!   "Convert an integer to string."
  integer, intent(in) :: k
  write (str, *) k
  str = adjustl(str)
end function str

logical function sta_exist(receiver, network, sta, ntw, sta_index)

    character(len=*) :: receiver, network
    character(len=*) :: sta(:), ntw(:)
    integer :: sta_index
    integer :: i

    if(sta_index.eq.0)then
      sta_exist=.false.
      return
    endif

    do i=1, sta_index
      if( (trim(sta(i)).eq.trim(receiver)) .and. &
                            (trim(ntw(i)).eq.trim(network)) ) then
      sta_exist=.true.
        return
      endif
    enddo

    sta_exist=.false.
    return

  end function sta_exist

  !subroutine that found the specific record
  !return the loc
  subroutine  locate_record(receiver, network, component, sta_array, &
        nw_array, comp_array, dim_array, loc)

    character(len=*) :: receiver, network, component
    character(len=*) :: sta_array(:), nw_array(:), comp_array(:)
    integer :: dim_array, loc
    integer :: i

    if(dim_array.eq.0)then
      loc=0
      return
    endif

    loc=0
    do i=1, dim_array
      if( (trim(sta_array(i)).eq.trim(receiver)) &
          .and. (trim(nw_array(i)).eq.trim(network)) )then
          if(trim(component).eq.trim(comp_array(i)(3:3)))then
            loc=i
          endif
      endif
    enddo

  end subroutine locate_record

end module process_subs
