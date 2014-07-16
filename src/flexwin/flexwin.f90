module flexwin_subs

contains
!----------------------------------------------------------------------
!> Modifiedy by Wenjie Lei
!> Change flexwin into a subroutine, called by the workflow
!> start from the very basic, meanign the input and output variables are basic

!> @param obs synt, obs and all the other info
!> @param npts2 num_win, win_start, win_end

!> @author Alesia(Original) Wenjie Lei()
  subroutine flexwin(obs_in, npts2_in, dt2_in, b2_in, &
                      synt_in, npts1_in, dt1_in, b1_in, &
                      evla_in, evlo_in, evdp_in, stla_in, stlo_in, &
                      kstnm_in, knetwk_in, kcmpnm_in, &
                      P_pick_in, S_pick_in, event_name,&
                      flexwin_par_all,win, REMOVE_SW_IN)

    use flexwin_struct 
    use user_parameters
    use seismo_variables
    use select_window_stalta_subs
    implicit none

    double precision,intent(in) :: synt_in(:), obs_in(:)
    double precision,intent(in) :: dt2_in, dt1_in, b2_in, b1_in
    integer,intent(in) :: npts2_in, npts1_in
    character(len=*),intent(in) :: kstnm_in, knetwk_in, kcmpnm_in
    double precision :: evla_in, evlo_in, stla_in, stlo_in, evdp_in
    double precision :: P_pick_in, S_pick_in
    character(len=*) :: event_name

    type(win_info) :: win
    type(flexwin_par_struct_all) :: flexwin_par_all
    logical :: REMOVE_SW_IN !remove surface wave to select window after it

    integer :: i
    integer :: ierr
    character(len=1000) :: basename, basedir !basename for extra ouput file
    character(len=20) :: P1, P2

    integer :: num_win_temp
    double precision :: ts_temp(NWINDOWS), te_temp(NWINDOWS)
    
    print *, "============"
    call copy_flexwin_par_to_local(flexwin_par_all, kcmpnm_in)
print *, kcmpnm_in, " check"
   DEBUG = .true. 
    if(DEBUG) write(*,*) "DEBUG: reading sac file"
    call copy_var_to_module_var(obs_in, npts2_in, dt2_in, b2_in, &
                      synt_in, npts1_in, dt1_in, b1_in, &
                      evla_in, evlo_in, evdp_in, stla_in, stlo_in, &
                      kstnm_in, knetwk_in, kcmpnm_in, &
                      P_pick_in, S_pick_in, REMOVE_SW_IN)

    write(*,*) "FLEXWIN--station, network, cmp: ", trim(kstnm), ".", trim(knetwk),&
          ".", trim(kcmpnm)
    if(DEBUG) then
      write(*,*) "obs npts:", npts2, " synt npts:", npts1
    endif

    !do this outside of this subroutine
    !call read_parameter_file()

    !do i=1,obsd_all%nrecords

     !check the consistency between obs and synt
    call check_and_filter_data(ierr)
    if(ierr.eq.1)then
      print *,"Data check failed. Skip this record now!"
      win%num_win=0
      return
    endif

    if (DEBUG) write(*,*) 'DEBUG : selecting windows'
    if(.not.REMOVE_SW)then
      !select window directly
      call select_windows_stalta2()
    else
      !first focus on body wave and surface wave
      print *, "STAGE1: FOCUS on BODY wave and SURFACE wave"
      FOCUS_PART=1
      call select_windows_stalta2()
      num_win_temp=num_win
      ts_temp(1:num_win)=win_start(1:num_win)
      te_temp(1:num_win)=win_end(1:num_win)
      print *,"STAGE1 num_win:", num_win
      !then focus on phases after surface wave
      !num_win=0
      !print *, "STAGE2: FOCUS on afterward phases"
      !FOCUS_PART=2
      !call select_windows_stalta2()
      !print *,"STAGE2 num_win:", num_win
      !!then combine two stage
      !call combine_windows(num_win_temp, ts_temp, te_temp, &
      !  num_win, win_start, win_end)
    endif
        !  if (DEBUG) write(*,*) 'DEBUG : writing output seismos'
        !  call write_seismos_gmt(basename(i))
        !endif

        !if(MAKE_WINDOW_FILES) then
        !  if (DEBUG) write(*,*) 'DEBUG : writing mt input'
        !  call write_mt_input_whole(OON,obs_name(i),syn_name(i))
        !endif

        !print *,"=============================="
        !print *,"window selection finished,write out win begins"
        !print *,num_win
        !print *,win_start(1:num_win)
        !print *,win_end(1:num_win)
    
        !print *,win%num_win
    win%num_win=num_win
    !print *,"write out num"
    if(num_win/=0)then
      allocate(win%t_start(num_win))
      allocate(win%t_end(num_win))
      win%t_start(1:num_win)=win_start(1:num_win)
      !print *,"write out start time"
      win%t_end(1:num_win)=win_end(1:num_win)
      !print *,"write out end time"
    endif


    !write out extra file
    write(P1,'(I3.3)') int(WIN_MIN_PERIOD)
    write(P2,'(I3.3)') int(WIN_MAX_PERIOD)
    basedir='./OUTPUT/'//trim(event_name)//"_"//trim(P1)&
        //"_"//trim(P2)
    call system('mkdir -p '//basedir//'')
    if(MAKE_SEISMO_PLOTS) then
      basename=trim(basedir)//"/"//trim(kstnm)//"."//trim(knetwk)//&
        "."//trim(kcmpnm)
      !print *, "basename:", trim(basename)
      call write_seismos_gmt(basename)
    endif

    !print *,"write out end"
    !print *,"=============================="

  end subroutine flexwin

!-----------------------------------------------------------------------
  subroutine copy_var_to_module_var(obs_in, npts2_in, dt2_in, b2_in, &
                  synt_in, npts1_in, dt1_in, b1_in, &
                  evla_in, evlo_in, evdp_in, stla_in, stlo_in, &
                  kstnm_in, knetwk_in, kcmpnm_in, &
                  P_pick_in, S_pick_in, REMOVE_SW_IN)

    use seismo_variables
    implicit none

    double precision, intent(in) :: synt_in(*), obs_in(*)
    double precision, intent(in) :: dt2_in, dt1_in, b2_in, b1_in
    integer,intent(in) :: npts2_in, npts1_in
    character(len=*),intent(in) :: kstnm_in, knetwk_in, kcmpnm_in 
    double precision :: evla_in, evlo_in, stla_in, stlo_in, evdp_in
    double precision :: P_pick_in, S_pick_in
    logical :: REMOVE_SW_IN

    integer :: string_len
        
    obs(1:npts2_in)=obs_in(1:npts2_in)
    npts2=npts2_in
    dt2=dt2_in
    b2=b2_in
    synt(1:npts1_in)=synt_in(1:npts1_in)
    npts1=npts1_in
    dt1=dt1_in
    b1=b1_in

    evla=real(evla_in)
    evlo=real(evlo_in)
    evdp=real(evdp_in)
    stla=real(stla_in)
    stlo=real(stlo_in)

    P_pick=real(P_pick_in)
    S_pick=real(S_pick_in)

    REMOVE_SW = REMOVE_SW_IN

    string_len=min(len(kstnm),len(kstnm_in))
    !print *,"string_len station",string_len
    kstnm(1:string_len)=kstnm_in(1:string_len)
    string_len=min(len(knetwk),len(knetwk_in))
    !print *,"string_len network",string_len
    knetwk(1:string_len)=knetwk_in(1:string_len)
    string_len=min(len(kcmpnm),len(kcmpnm_in))
    !print *,"string_len component",string_len
    kcmpnm(1:string_len)=kcmpnm_in(1:string_len)
    !print *,
    !print *,trim(kcmpnm)

  end subroutine copy_var_to_module_var

  !subroutine copy_global_to_local(obsd_all,synt_all,loc)
  
  !type(asdf_event),intent(in) :: obsd_all,synt_all
  !integer :: loc
  !copy
  !synt data info
  !dt1=synt_all%sample_rate(loc)
  !npts1=synt_all%npoints(loc)
  !b1=synt_all%begin_value(loc)
  !synt(1:npts1)=synt_all%records(loc)%record(1:npts1)
  
  !obs data info
  !dt2=obsd_all%sample_rate(loc)
  !npts2=obsd_all%npoints(loc)
  !b2=obsd_all%begin_value(loc)
  !obs(1:npts2)=obsd_all%records(loc)%record(1:npts2)

  !location info
  !evla=obsd_all%event_lat
  !evlo=obsd_all%event_lo
  !evdp=obsd_all%event_dpt

  !stla=obsd_all%receiver_lat(loc)
  !stlo=obsd_all%receiver_lo(loc)

  !station info
  !kstnm=obsd_all%receiver_name(loc)
  !knetwk=obsd_all%network(loc)
  !kcmpnm=obsd_all%component(loc)


  !P_arrival and S_arrival
  !P_pick=obsd_all%P_pick(loc)
  !S_pick=obsd_all%S_pick(loc)

  !end subroutine copy_global_to_local

  subroutine copy_flexwin_par_to_local(flexwin_par_all,kcmpnm_in)

    use flexwin_struct 
    implicit none

    type(flexwin_par_struct_all) :: flexwin_par_all
    character(len=*),intent(in) :: kcmpnm_in

    !print *, "kcmpnm:", trim(kcmpnm)
    if(kcmpnm_in(3:3)=="Z") then
      call copy_flexwin_par_to_local_sub(flexwin_par_all%Z)
    elseif(kcmpnm_in(3:3)=="R")then
      call copy_flexwin_par_to_local_sub(flexwin_par_all%R)
    elseif(kcmpnm_in(3:3)=="T")then
      call copy_flexwin_par_to_local_sub(flexwin_par_all%T)
    else
      stop
    endif

  end subroutine copy_flexwin_par_to_local

  subroutine copy_flexwin_par_to_local_sub(flexwin_par)
   
    use flexwin_struct 
    use seismo_variables
    implicit none

    type(flexwin_par_struct) :: flexwin_par

    !copy
    DEBUG             = flexwin_par%DEBUG
    print *, "FLEXWIN DEBUG:", DEBUG
    MAKE_SEISMO_PLOTS = flexwin_par%MAKE_SEISMO_PLOTS
    MAKE_WINDOW_FILES = flexwin_par%MAKE_WINDOW_FILES
    BODY_WAVE_ONLY    = flexwin_par%BODY_WAVE_ONLY
    RUN_BANDPASS      = flexwin_par%RUN_BANDPASS
    WIN_MIN_PERIOD    = flexwin_par%WIN_MIN_PERIOD
    WIN_MAX_PERIOD    = flexwin_par%WIN_MAX_PERIOD
    FSTART   = flexwin_par%FSTART
    FEND     = flexwin_par%FEND
    STALTA_BASE       = flexwin_par%STALTA_BASE
    TSHIFT_BASE       = flexwin_par%TSHIFT_BASE
    TSHIFT_REFERENCE  = flexwin_par%TSHIFT_REFERENCE
    DLNA_BASE         = flexwin_par%DLNA_BASE
    DLNA_REFERENCE    = flexwin_par%DLNA_REFERENCE
    CC_BASE           = flexwin_par%CC_BASE
    DATA_QUALITY      = flexwin_par%DATA_QUALITY
    SNR_INTEGRATE_BASE = flexwin_par%SNR_INTEGRATE_BASE
    SNR_MAX_BASE      = flexwin_par%SNR_MAX_BASE
    WINDOW_S2N_BASE   = flexwin_par%WINDOW_S2N_BASE
    C_0  = flexwin_par%C_0
    C_1  = flexwin_par%C_1
    C_2  = flexwin_par%C_2
    C_3a = flexwin_par%C_3a
    C_3b = flexwin_par%C_3b
    C_4a = flexwin_par%C_4a
    C_4b = flexwin_par%C_4b
    WEIGHT_SPACE_COVERAGE = flexwin_par%WEIGHT_SPACE_COVERAGE
    WEIGHT_AVERAGE_CC = flexwin_par%WEIGHT_AVERAGE_CC
    WEIGHT_N_WINDOWS  = flexwin_par%WEIGHT_N_WINDOWS

  end subroutine copy_flexwin_par_to_local_sub


  subroutine combine_windows(nw1,ts1,te1,nw2,ts2,te2)

  use user_parameters, only : NWINDOWS

  integer :: nw1, nw2, nw
  double precision, dimension(:) :: ts1, te1, ts2, te2
  double precision, dimension(NWINDOWS) :: ts, te
  integer :: i

  nw=nw1+nw2
  ts(1:nw1)=ts1(1:nw1)
  te(1:nw1)=te1(1:nw1)
  
  do i=1,nw2
    ts(i+nw1)=ts2(i)
    te(i+nw1)=ts2(i)
  enddo

  nw2=nw
  ts2(1:nw)=ts(1:nw)
  te2(1:nw)=te(1:nw)

  end subroutine combine_windows
  
!----------------------------------------------------------------------
end module flexwin_subs
