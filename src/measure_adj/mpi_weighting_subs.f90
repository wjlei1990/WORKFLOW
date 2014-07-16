module mpi_weighting_subs

contains

!-------------------------------------------------.
!set up weigthing par, called by main program     !
!-------------------------------------------------'
subroutine setup_measure_adj_weighting_asdf_mpi(win,npairs,dist,chan_array,&
                  ma_weighting_par, weighting_option,&
                  rank, comm, ierr)
  
  !window count
  use flexwin_struct
  use ma_struct
  use ma_weighting

  use ma_constants, only : NDIM
  use ma_sub2, only : TOL
  !use ma_weighting, only : surface_vel

  implicit none

  include 'mpif.h'

  type(win_info),intent(in) :: win(:)
  integer,intent(in) :: npairs
  character(len=*), intent(in) :: chan_array(:)
  double precision,intent(in) :: dist(:)
  integer, intent(in) :: comm, rank
  integer, intent(in) :: weighting_option !actually this hasn't been used here

	integer :: i

  type(ma_weighting_par_struct) :: ma_weighting_par

  double precision :: num_P_SV_V_total=0., num_P_SV_R_total=0.
  double precision :: num_SH_T_total=0., num_Love_T_total=0.
  double precision :: num_Rayleigh_V_total=0., num_Rayleigh_R_total=0.
  
  integer :: ierr
  !
  ! determines weights based on number of window picks on Z/R/T components
  !

  !character(len=10),intent(in) :: chan_syn

  ! local parameters
  !integer :: npairs,ios,ipair,iposition,ipicks
  integer :: ios,ipair,iposition,ipicks
  !character(len=150) :: datafile,synfile !,dummy
  character(len=4) :: comp_T,comp_Z,comp_R
  integer :: picks_T, picks_Z, picks_R,npicks
  ! sac header information
  !integer :: yr,jda,ho,mi
  !double precision :: sec,dist,az,baz,slat,slon,T_surfacewaves
  double precision :: T_surfacewaves
  character(len=10) :: net,sta,chan_dat,chan,cmp
  double precision :: t01, dt1, t02, dt2, t0, dt, tstart, tend
  integer :: npt1, npt2, npts
  !double precision, dimension(NDIM) :: data, syn

  !call get_sacfile_header(trim(datafile),yr,jda,ho,mi,sec,net,sta, &
  !                        chan_dat,dist,az,baz,slat,slon)
  ! initializes

  print *,"dist:"
  print *, dist(:)

  !print *, chan_array(:)(:)

  picks_R = 0
  picks_Z = 0
  picks_T = 0

  num_P_SV_V = 0.d0
  num_P_SV_R = 0.d0
  num_SH_T = 0.d0

  num_Rayleigh_V = 0.d0
  num_Rayleigh_R = 0.d0
  num_Love_T = 0.d0

  ! substrings (synthetics components)
  !comp_T = trim(chan_syn)//"T."
  !comp_R = trim(chan_syn)//"R."
  !comp_Z = trim(chan_syn)//"Z."

  ! opens measurement windows
  !open(21,file='MEASUREMENT.WINDOWS',status='old',iostat=ios)
  !if (ios /= 0) stop 'Error opening input file: MEASUREMENT WINDOWS'
  !read(21,*,iostat=ios) npairs
  !if (ios /= 0) stop 'Error reading number of pairs of data/syn'
  ! loops through windows
  do ipair=1, npairs

    ! reads in file names
    !read(21,'(a)',iostat=ios) datafile
    !if (ios /= 0) stop 'Error reading windows datafile'
    !read(21,'(a)',iostat=ios) synfile
    !if (ios /= 0) stop 'Error reading windows synfile'

    ! read data and syn (read datafile last to take its header later)
    !call drsac1(synfile,syn,npt2,t02,dt2)
    !call drsac1(datafile,data,npt1,t01,dt1)

    !if (max(npt1,npt2) > NDIM) &
    !    stop 'Error: Too many npts in data or syn'
    
    ! check if t0 and dt match
    !if (abs(dt1-dt2) > TOL) stop 'Error: check if dt match'
    !dt = dt1
    !npts = min(npt1,npt2)
    !if (abs(t01-t02) > dt) then
    !  print *,'data t0: ',t01
    !  print *,'syn  t0: ',t02
    !  stop 'Check if t0 match'
    !endif
    !t0 = t01

    ! figure out station/network/comp names, etc
    !call get_sacfile_header(trim(datafile),yr,jda,ho,mi,sec,net,sta, &
    !                        chan_dat,dist,az,baz,slat,slon)
    chan = chan_array(ipair)
    cmp = chan_array(ipair)(3:3)

    ! theoretical surface wave arrival time
    T_surfacewaves = dist(ipair)/180.0*3.1415*6400 / surface_vel

    print *,"T_surfacewaves:", T_surfacewaves

    ! debug output
    !if (DISPLAY_DETAILS) then
    !print*,'debug: '
    !print*,'  yr,jda,ho,mi,sec : ',yr,jda,ho,mi,sec
    !print*,'  net,sta,chan_dat : ',net,sta,chan_dat
    !print*,'  dist,az,baz,slat,slon : ',dist,az,baz,slat,slon
    !print*,'  cmp          = ',cmp
    !print*,'  dist           = ',dist
    !print*,'  T_surfacewaves = ',T_surfacewaves
    !print*
    !endif

    ! reads in window picks
    !read(21,*,iostat=ios) npicks
    !if (ios /= 0) stop 'Error reading windows npicks'
    npicks=win(ipair)%num_win
    print *, "npair, npicks:", npairs, npicks

    ! loops/skips over picks (start/end times)
    do ipicks=1,npicks

      !read(21,*,iostat=ios) tstart, tend
      !if (ios /= 0) stop 'Error reading window pick: tstart and tend'
      tstart=win(ipair)%t_start(ipicks)
      tend=win(ipair)%t_end(ipicks)

      !tstart = max(tstart,t0)
      !tend = min(tend, t0+(npts-1)*dt)
      !print *, "ipair, ipick:", ipair, ipicks

      ! body wave picks
      if( tend <= T_surfacewaves ) then
        if( cmp(1:1) == "Z" ) num_P_SV_V = num_P_SV_V + 1.d0
        if( cmp(1:1) == "R" ) num_P_SV_R = num_P_SV_R + 1.d0
        if( cmp(1:1) == "T" ) num_SH_T = num_SH_T + 1.d0
      else
      ! surface wave picks
        if( cmp(1:1) == "Z" ) num_Rayleigh_V = num_Rayleigh_V + 1.d0
        if( cmp(1:1) == "R" ) num_Rayleigh_R = num_Rayleigh_R + 1.d0
        if( cmp(1:1) == "T" ) num_Love_T = num_Love_T + 1.d0
      endif

    enddo !end of npicks

    ! determines all picks on a trace component 
    ! (also cross-check comp name in filename)
    ! transverse
    !iposition = INDEX( trim(synfile), comp_T, .false. )
    !if( iposition > 3 .and. iposition < len_trim( synfile) ) then
    !  if( cmp(1:1) /= "T" ) stop 'error T component pick'
    if(cmp(1:1) == "T") then
      picks_T = picks_T + npicks
    !else
      ! radial
      !iposition = INDEX( trim(synfile), comp_R, .false. )
      !if( iposition > 3 .and. iposition < len_trim( synfile) ) then
        !if( cmp(1:1) /= "R" ) stop 'error R component pick'
    elseif(cmp(1:1) == "R") then 
      picks_R = picks_R + npicks
     ! else
        ! vertical
        !iposition = INDEX( trim(synfile), comp_Z, .false. )
        !if( iposition > 3 .and. iposition < len_trim( synfile) ) then
          !if( cmp(1:1) /= "Z" ) stop 'error Z component pick'
    elseif(cmp(1:1) == "Z") then
      picks_Z = picks_Z + npicks
    endif

  enddo ! end of npairs

  !close(21)

  print*
  print*,'weighting measurements: '
  print*,'  picks T:',picks_T
  print*,'  picks R:',picks_R
  print*,'  picks Z:',picks_Z
  print*
  print*,'  picks P_SV_R: ',nint(num_P_SV_R)
  print*,'  picks P_SV_V: ',nint(num_P_SV_V)
  print*,'  picks SH_T  : ',nint(num_SH_T)
  print*,'  picks Rayleigh_R: ',nint(num_Rayleigh_R)
  print*,'  picks Rayleigh_V: ',nint(num_Rayleigh_V)
  print*,'  picks Love_T    : ',nint(num_Love_T)
  print*

  ! check with total number of picks per component
  if( nint( num_P_SV_R + num_Rayleigh_R ) /= picks_R ) stop 'error R picks'
  if( nint( num_P_SV_V + num_Rayleigh_V ) /= picks_Z ) stop 'error Z picks'
  if( nint( num_SH_T + num_Love_T ) /= picks_T ) stop 'error T picks'

	!call MPI_Barrier(comm, ierr)
	!do i=1, 10000000*rank
	!enddo

  !if( DISPLAY_DETAILS ) then
    print*
    print*,'weighting measurements: '
    print*,'  picks T:',picks_T
    print*,'  picks R:',picks_R
    print*,'  picks Z:',picks_Z
    print*
    print*,'  picks P_SV_R: ',nint(num_P_SV_R)
    print*,'  picks P_SV_V: ',nint(num_P_SV_V)
    print*,'  picks SH_T  : ',nint(num_SH_T)
    print*,'  picks Rayleigh_R: ',nint(num_Rayleigh_R)
    print*,'  picks Rayleigh_V: ',nint(num_Rayleigh_V)
    print*,'  picks Love_T    : ',nint(num_Love_T)
    print*
  !endif

  !-------------------.
  !Gather from local  !
  !-------------------'
  call MPI_Reduce(num_P_SV_R,num_P_SV_R_total,1, MPI_DOUBLE_PRECISION,&
              MPI_SUM,0,comm,ierr)
  call MPI_Reduce(num_P_SV_V,num_P_SV_V_total,1, MPI_DOUBLE_PRECISION,&
              MPI_SUM,0,comm,ierr)
  call MPI_Reduce(num_SH_T,num_SH_T_total,1, MPI_DOUBLE_PRECISION,&
              MPI_SUM,0,comm,ierr)
  call MPI_Reduce(num_Rayleigh_R,num_Rayleigh_R_total,1, MPI_DOUBLE_PRECISION,&
              MPI_SUM,0,comm,ierr)
  call MPI_Reduce(num_Rayleigh_V,num_Rayleigh_V_total,1, MPI_DOUBLE_PRECISION,&
              MPI_SUM,0,comm,ierr)
  call MPI_Reduce(num_Love_T,num_Love_T_total,1, MPI_DOUBLE_PRECISION,&
              MPI_SUM,0,comm,ierr)

  !-------------------.
  !Bcast to local     !
  !-------------------'
  call MPI_Bcast(num_P_SV_R_total, 1, MPI_DOUBLE_PRECISION, 0, comm, ierr)
  call MPI_Bcast(num_P_SV_V_total, 1, MPI_DOUBLE_PRECISION, 0, comm, ierr)
  call MPI_Bcast(num_SH_T_total, 1, MPI_DOUBLE_PRECISION, 0, comm, ierr)
  call MPI_Bcast(num_Rayleigh_R_total, 1, MPI_DOUBLE_PRECISION, 0, comm, ierr)
  call MPI_Bcast(num_Rayleigh_V_total, 1, MPI_DOUBLE_PRECISION, 0, comm, ierr)
  call MPI_Bcast(num_Love_T_total, 1, MPI_DOUBLE_PRECISION, 0, comm, ierr)

	!call MPI_Barrier(comm, ierr)
	!do i=1, 10000000*rank
	!enddo
	!print *, num_P_SV_R_total, num_P_SV_V_total
	!print *, num_SH_T_total, num_Love_T_total
	!print *, num_Rayleigh_R_total, num_Rayleigh_V_total

  ! sets up weights based on picks
  weight_T = 1.0d0
  weight_R = 1.0d0
  weight_Z = 1.0d0

  ! weighting tries to balance love waves (tranverse) versus rayleigh waves (radial + vertical)
  !if( picks_T > 0 ) then
  !  if( picks_R + picks_Z > 0 ) weight_T = dble(picks_R + picks_Z)/dble(picks_T)
  !endif
  picks_T=num_SH_T_total+num_Love_T_total
  picks_R=num_P_SV_R_total+num_Rayleigh_R_total
  picks_Z=num_P_SV_V_total+num_Rayleigh_V_total

  ! use normalization as weights
  if( picks_T > 0 ) weight_T = 1.d0 / picks_T
  if( picks_R > 0 ) weight_R = 1.d0 / picks_R
  if( picks_Z > 0 ) weight_Z = 1.d0 / picks_Z

  ! use normalization (no traces means zero weights)
  if( num_P_SV_R_total > 0. ) num_P_SV_R = 1.d0 / num_P_SV_R_total
  if( num_P_SV_V_total > 0. ) num_P_SV_V = 1.d0 / num_P_SV_V_total
  if( num_SH_T_total > 0. ) num_SH_T = 1.d0 / num_SH_T_total
  if( num_Rayleigh_R_total > 0. ) num_Rayleigh_R = 1.d0 / num_Rayleigh_R_total
  if( num_Rayleigh_V_total > 0. ) num_Rayleigh_V = 1.d0 / num_Rayleigh_V_total
  if( num_Love_T_total > 0. ) num_Love_T = 1.d0 / num_Love_T_total

  print*,'  weight of P_SV_R:',num_P_SV_R
  print*,'  weight of P_SV_V:',num_P_SV_V
  print*,'  weight of SH_T  :',num_SH_T
  print*,'  weight of Rayleigh_R:',num_Rayleigh_R
  print*,'  weight of Rayleigh_V:',num_Rayleigh_V
  print*,'  weight of Love_T:', num_Love_T

  !copy all the var to struct
  ma_weighting_par%weight_T=weight_T
  ma_weighting_par%weight_R=weight_R
  ma_weighting_par%weight_Z=weight_Z
  ma_weighting_par%num_P_SV_R=num_P_SV_R
  ma_weighting_par%num_P_SV_V=num_P_SV_V
  ma_weighting_par%num_SH_T=num_SH_T
  ma_weighting_par%num_Rayleigh_R=num_Rayleigh_R
  ma_weighting_par%num_Rayleigh_V=num_Rayleigh_V
  ma_weighting_par%num_Love_T=num_Love_T

end subroutine setup_measure_adj_weighting_asdf_mpi


end module mpi_weighting_subs
