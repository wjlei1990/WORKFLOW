! -------------------------------------------------------------
! edit here to change T0 and T1 on some condition 
! Note, this function is called AFTER the seismogram has been 
! read but before it is filtered.
! -------------------------------------------------------------

subroutine modify_T0_T1_on_condition
  use seismo_variables

  ! do nothing

  ! adjust fstart and fend accordingly
  FSTART=1./WIN_MAX_PERIOD
  FEND=1./WIN_MIN_PERIOD

  end subroutine


subroutine set_up_criteria_arrays
	use seismo_variables
	implicit none

  call set_up_criteria_arrays_default

	!based on component info and period info to choose specific criteria
	if(WIN_MAX_PERIOD.le.80.0 .and. WIN_MIN_PERIOD.ge.5.0)then
		!body wave
		select case (kcmpnm(3:3))
			case ("Z")
				call set_up_criteria_arrays_bw_Z
			case ("R")
				call set_up_criteria_arrays_bw_R
			case ("T")
				call set_up_criteria_arrays_bw_T
			case DEFAULT
				write(*,*) "Specific set_up_criteria subroutine missing."
				write(*,*) "COMPONENT: ", kcmpnm(3:3), &
						" PERIOD: ", WIN_MAX_PERIOD, WIN_MIN_PERIOD
				write(*,*) "Create your own if you want to continuew"
				stop
		end select
	else if(WIN_MAX_PERIOD.le.140.0 .and. WIN_MIN_PERIOD.ge.50.0) then
		!surface wave
		select case (kcmpnm(3:3))
			case ("Z")
				call set_up_criteria_arrays_sw_Z
			case ("R")
				call set_up_criteria_arrays_sw_R
			case ("T")
				call set_up_criteria_arrays_sw_T
			case DEFAULT
				write(*,*) "Specific set_up_criteria subroutine missing."
				write(*,*) "COMPONENT: ", kcmpnm(3:3), &
						" PERIOD: ", WIN_MAX_PERIOD, WIN_MIN_PERIOD
				write(*,*) "Create your own if you want to continuew"
				stop
		end select
	else
		!none file found
		write(*,*) "Specific set_up_criteria subroutine missing."
		write(*,*) "COMPONENT: ", kcmpnm(3:3), &
				" PERIOD: ", WIN_MAX_PERIOD, WIN_MIN_PERIOD
		write(*,*) "Create your own if you want to continuew"
		stop
	endif

end subroutine


! -------------------------------------------------------------
! edit here to change the time dependent properties of the 
! selection criteria
! Note, this function is called AFTER the seismogram has been 
! read and filtered.
! -------------------------------------------------------------

subroutine set_up_criteria_arrays_bw_R
  use seismo_variables 

  integer :: i
  double precision :: time
  double precision :: R_vel, R_time
  double precision :: Q_vel, Q_time

! -----------------------------------------------------------------
! Start of user-dependent portion

! This is where you reset the signal_end and noise_end values
! if you want to modify them from the defaults.
! This is also where you modulate the time dependence of the selection
! criteria.  You have access to the following parameters from the 
! seismogram itself:
!
! dt, b, kstnm, knetwk, kcmpnm
! evla, evlo, stla, stlo, evdp, azimuth, backazimuth, dist_deg, dist_km
! num_phases, ph_names, ph_times
!
! Example of modulation:
!-----------------------
! To increase s2n limit after arrival of R1 try
!
! R_vel=3.2
! R_time=dist_km/R_vel
! do i = 1, npts
!   time=b+(i-1)*dt
!   if (time.gt.R_time) then
!     S2N_LIMIT(i)=2*WINDOW_S2N_BASE
!   endif
! enddo
!

 ! --------------------------------
 ! Set approximate end of rayleigh wave arrival
 R_vel=4.0
 R_time=dist_km/R_vel
 ! --------------------------------
 ! Set approximate start of love wave arrival
 !Q_vel=4.2
 Q_vel=4.5
 Q_time=dist_km/Q_vel

 ! reset the signal_end time to be the end of the Rayleigh waves
 if (DATA_QUALITY) then
   signal_end=R_time
 endif
  !noise_end  = P_pick - 0.1*P_pick ! vertical and radial comp.
  ! noise_end  = S_pick - 0.2*S_pick ! transverse comp.
  !noise_start = b


 ! --------------------------------
 ! modulate criteria in time
 do i = 1, npts
   time=b+(i-1)*dt
   ! --------------------------------
   ! if we are beyond the Rayleigh wave, then make the all criteria stronger
   ! ratio criterion stronger
   if(evdp.gt.200.0)then  
    if ( (time.gt.R_time) .and. (time.lt.(0.2*(CIRCUM_EARTH/2)/R_vel)) ) then
     S2N_LIMIT(i)=2*WINDOW_S2N_BASE    ! only pick big signals
     !CC_LIMIT(i)= 0.95                  ! only pick very similar signals
     !TSHIFT_LIMIT(i)=TSHIFT_BASE/2.0    ! only pick small timeshifts
     !DLNA_LIMIT(i)=DLNA_BASE/2.0        ! only pick small amplitude anomalies
     STALTA_W_LEVEL(i)=STALTA_BASE*2.0     ! pick only distinctive arrivals
    endif

    if( time.gt.(0.2*(CIRCUM_EARTH/2)/R_vel) .and. &
        (time.gt.R_time) ) then
     S2N_LIMIT(i)=10*WINDOW_S2N_BASE    ! only pick big signals
     CC_LIMIT(i)= 0.95                  ! only pick very similar signals
     TSHIFT_LIMIT(i)=TSHIFT_BASE/2.0    ! only pick small timeshifts
     DLNA_LIMIT(i)=DLNA_BASE/3.0        ! only pick small amplitude anomalies
     STALTA_W_LEVEL(i)=STALTA_BASE*10    ! pick only distinctive arrivals
    endif
  else
    !shallow earthquake
    if( (time.gt.R_time) ) then
     S2N_LIMIT(i)=10*WINDOW_S2N_BASE    ! only pick big signals
     CC_LIMIT(i)= 0.95                  ! only pick very similar signals
     TSHIFT_LIMIT(i)=TSHIFT_BASE/2.0    ! only pick small timeshifts
     DLNA_LIMIT(i)=DLNA_BASE/3.0        ! only pick small amplitude anomalies
     STALTA_W_LEVEL(i)=STALTA_BASE*10    ! pick only distinctive arrivals
    endif
  endif

   ! --------------------------------
   ! if we are in the surface wave times, then make the cross-correlation
   ! criterion less severe
   !!if (time.gt.Q_time .and. time.lt.R_time) then
   !! CC_LIMIT(i)=0.9*CC_LIMIT(i)
   !!endif

   ! for source inversion increase S2N ratio
   ! particularly to reject low amplitude signal comparable to noise

   ! reject windows before P or S arrivals
   !if (time.lt.(S_pick - 0.2*S_pick)) then
   if (time.lt.noise_end) then
     STALTA_W_LEVEL(i)=1     ! to avoid windows before P or S arrivals
   endif

   ! select only body waves
   !if (time.gt.Q_time) then
   !  STALTA_W_LEVEL(i)=1
   !endif


   ! --------------------------------
   ! modulate criteria according to event depth
   !
   ! if an intermediate depth event
   !!UNCOMMENT THE LINES BELOW IN CASE OF SOURCE INVERSIONS
!   if (evdp.ge.100 .and. evdp.lt.300) then
  !   TSHIFT_LIMIT(i)=TSHIFT_BASE*1.4
  ! ! if a deep event
  ! elseif (evdp.ge.300) then
  !   TSHIFT_LIMIT(i)=TSHIFT_BASE*1.7
!   endif
 enddo
!
! End of user-dependent portion
! -----------------------------------------------------------------
end subroutine
! -------------------------------------------------------------

subroutine set_up_criteria_arrays_bw_T
  use seismo_variables 

  integer :: i
  double precision :: time
  double precision :: R_vel, R_time
  double precision :: Q_vel, Q_time


! -----------------------------------------------------------------
! Start of user-dependent portion

! This is where you reset the signal_end and noise_end values
! if you want to modify them from the defaults.
! This is also where you modulate the time dependence of the selection
! criteria.  You have access to the following parameters from the 
! seismogram itself:
!
! dt, b, kstnm, knetwk, kcmpnm
! evla, evlo, stla, stlo, evdp, azimuth, backazimuth, dist_deg, dist_km
! num_phases, ph_names, ph_times
!
! Example of modulation:
!-----------------------
! To increase s2n limit after arrival of R1 try
!
! R_vel=3.2
! R_time=dist_km/R_vel
! do i = 1, npts
!   time=b+(i-1)*dt
!   if (time.gt.R_time) then
!     S2N_LIMIT(i)=2*WINDOW_S2N_BASE
!   endif
! enddo
!

 ! --------------------------------
 ! Set approximate end of rayleigh wave arrival
 R_vel=4.0
 R_time=dist_km/R_vel
 ! --------------------------------
 ! Set approximate start of love wave arrival
 !Q_vel=4.2
 Q_vel=4.5
 Q_time=dist_km/Q_vel

 ! reset the signal_end time to be the end of the Rayleigh waves
 if (DATA_QUALITY) then
   signal_end=R_time
 endif
  !noise_end  = P_pick - 0.1*P_pick ! vertical and radial comp.
  ! noise_end  = S_pick - 0.2*S_pick ! transverse comp.
  !noise_start = b


 ! --------------------------------
 ! modulate criteria in time
 do i = 1, npts
   time=b+(i-1)*dt
   ! --------------------------------
   ! if we are beyond the Rayleigh wave, then make the all criteria stronger
   ! ratio criterion stronger
   if(evdp.gt.200.0)then
    !deep earthquake: ScS
    if ( (time.gt.R_time) .and. (time.lt.(0.5*(CIRCUM_EARTH/2)/R_vel)) ) then
      S2N_LIMIT(i)=3*WINDOW_S2N_BASE    ! only pick big signals
      CC_LIMIT(i)= CC_BASE+0.03                 ! only pick very similar signals
      TSHIFT_LIMIT(i)=TSHIFT_BASE/1.5    ! only pick small timeshifts
      DLNA_LIMIT(i)=DLNA_BASE/1.5        ! only pick small amplitude anomalies
      STALTA_W_LEVEL(i)=STALTA_BASE*1.5     ! pick only distinctive arrivals
    endif
    if( (time.gt.(0.5*(CIRCUM_EARTH/2)/R_vel)) .and. &
       (time.gt.R_time)  ) then
     S2N_LIMIT(i)=10*WINDOW_S2N_BASE    ! only pick big signals
     CC_LIMIT(i)= 0.95                  ! only pick very similar signals
     TSHIFT_LIMIT(i)=TSHIFT_BASE/2.0    ! only pick small timeshifts
     DLNA_LIMIT(i)=DLNA_BASE/2.0        ! only pick small amplitude anomalies
     STALTA_W_LEVEL(i)=STALTA_BASE*5.0     ! pick only distinctive arrivals
    endif
  else
    !shallow earthquake: no ScS
    if( time.gt.R_time )then
     S2N_LIMIT(i)=10*WINDOW_S2N_BASE    ! only pick big signals
     CC_LIMIT(i)= 0.95                  ! only pick very similar signals
     TSHIFT_LIMIT(i)=TSHIFT_BASE/2.0    ! only pick small timeshifts
     DLNA_LIMIT(i)=DLNA_BASE/2.0        ! only pick small amplitude anomalies
     STALTA_W_LEVEL(i)=STALTA_BASE*5.0     ! pick only distinctive arrivals
   endif
  endif

   ! --------------------------------
   ! if we are in the surface wave times, then make the cross-correlation
   ! criterion less severe
   !!if (time.gt.Q_time .and. time.lt.R_time) then
   !! CC_LIMIT(i)=0.9*CC_LIMIT(i)
   !!endif

   ! for source inversion increase S2N ratio
   ! particularly to reject low amplitude signal comparable to noise

   ! reject windows before P or S arrivals
   !if (time.lt.(S_pick - 0.2*S_pick)) then
   if (time.lt.noise_end) then
     STALTA_W_LEVEL(i)=1     ! to avoid windows before P or S arrivals
   endif

   ! select only body waves
   !if (time.gt.Q_time) then
   !  STALTA_W_LEVEL(i)=1
   !endif


   ! --------------------------------
   ! modulate criteria according to event depth
   !
   ! if an intermediate depth event
   !!UNCOMMENT THE LINES BELOW IN CASE OF SOURCE INVERSIONS
!   if (evdp.ge.100 .and. evdp.lt.300) then
  !   TSHIFT_LIMIT(i)=TSHIFT_BASE*1.4
  ! ! if a deep event
  ! elseif (evdp.ge.300) then
  !   TSHIFT_LIMIT(i)=TSHIFT_BASE*1.7
!   endif
 enddo
!
! End of user-dependent portion
! -----------------------------------------------------------------
end subroutine


! -------------------------------------------------------------
! edit here to change the time dependent properties of the 
! selection criteria
! Note, this function is called AFTER the seismogram has been 
! read and filtered.
! -------------------------------------------------------------

subroutine set_up_criteria_arrays_bw_Z
  use seismo_variables 

  integer :: i
  double precision :: time
  double precision :: R_vel, R_time
  double precision :: Q_vel, Q_time


! -----------------------------------------------------------------
! Start of user-dependent portion

! This is where you reset the signal_end and noise_end values
! if you want to modify them from the defaults.
! This is also where you modulate the time dependence of the selection
! criteria.  You have access to the following parameters from the 
! seismogram itself:
!
! dt, b, kstnm, knetwk, kcmpnm
! evla, evlo, stla, stlo, evdp, azimuth, backazimuth, dist_deg, dist_km
! num_phases, ph_names, ph_times
!
! Example of modulation:
!-----------------------
! To increase s2n limit after arrival of R1 try
!
! R_vel=3.2
! R_time=dist_km/R_vel
! do i = 1, npts
!   time=b+(i-1)*dt
!   if (time.gt.R_time) then
!     S2N_LIMIT(i)=2*WINDOW_S2N_BASE
!   endif
! enddo
!

 ! --------------------------------
 ! Set approximate end of rayleigh wave arrival
 R_vel=4.0
 R_time=dist_km/R_vel
 ! --------------------------------
 ! Set approximate start of love wave arrival
 Q_vel=4.5
 !Q_vel=5.0
 Q_time=dist_km/Q_vel

 ! reset the signal_end time to be the end of the Rayleigh waves
 if (DATA_QUALITY) then
   signal_end=R_time
 endif
  !noise_end  = P_pick - 0.1*P_pick ! vertical and radial comp.
  !noise_end  = S_pick - 0.2*S_pick ! transverse comp.
  ! noise_start = b


 ! --------------------------------
 ! modulate criteria in time
 do i = 1, npts
   time=b+(i-1)*dt
   ! --------------------------------
   ! if we are beyond the Rayleigh wave, then make the all criteria stronger
   ! ratio criterion stronger
   !if (time.gt.R_time) then
   !  S2N_LIMIT(i)=10*WINDOW_S2N_BASE    ! only pick big signals
   !  CC_LIMIT(i)= 0.95                  ! only pick very similar signals
   !  TSHIFT_LIMIT(i)=TSHIFT_BASE/3.0    ! only pick small timeshifts
   !  DLNA_LIMIT(i)=DLNA_BASE/3.0        ! only pick small amplitude anomalies
   !  STALTA_W_LEVEL(i)=STALTA_BASE*2.0     ! pick only distinctive arrivals
   !endif

   !if ( (time.gt.R_time) .and. (time.lt.(CIRCUM_EARTH*0.5/R_vel)) ) then
   !  S2N_LIMIT(i)=5*WINDOW_S2N_BASE    ! only pick big signals
   !  CC_LIMIT(i)= 0.95                  ! only pick very similar signals
   !  !TSHIFT_LIMIT(i)=TSHIFT_BASE/2.0    ! only pick small timeshifts
   !  !DLNA_LIMIT(i)=DLNA_BASE/2.0        ! only pick small amplitude anomalies
   !  STALTA_W_LEVEL(i)=STALTA_BASE*2.0     ! pick only distinctive arrivals
   !endif

   if(time.gt.(R_time)) then
     S2N_LIMIT(i)=5*WINDOW_S2N_BASE    ! only pick big signals
     CC_LIMIT(i)= 0.95                  ! only pick very similar signals
     TSHIFT_LIMIT(i)=TSHIFT_BASE/2.0    ! only pick small timeshifts
     DLNA_LIMIT(i)=DLNA_BASE/2.0        ! only pick small amplitude anomalies
     STALTA_W_LEVEL(i)=STALTA_BASE*2.0     ! pick only distinctive arrivals
   endif
   ! --------------------------------
   ! if we are in the surface wave times, then make the cross-correlation
   ! criterion less severe
   !!if (time.gt.Q_time .and. time.lt.R_time) then
   !! CC_LIMIT(i)=0.9*CC_LIMIT(i)
   !!endif

   ! for source inversion increase S2N ratio
   ! particularly to reject low amplitude signal comparable to noise

   ! reject windows before P or S arrivals
   if (time.lt.noise_end) then
     STALTA_W_LEVEL(i)=1     ! to avoid windows before P or S arrivals
   endif

   !select only body waves
   if (time.gt.R_time) then
     STALTA_W_LEVEL(i)=1
   endif


   ! --------------------------------
   ! modulate criteria according to event depth
   !
   !! UNCOMMENT THE LINES BELOW IN CASE OF SOURCE INVERSIONS!!
   ! if an intermediate depth event
!   if (evdp.ge.100 .and. evdp.lt.300) then
!     TSHIFT_LIMIT(i)=TSHIFT_BASE*1.4
   ! if a deep event
!   elseif (evdp.ge.300) then
!     TSHIFT_LIMIT(i)=TSHIFT_BASE*1.7
!   endif
 enddo

!
! End of user-dependent portion
! -----------------------------------------------------------------
end subroutine
! -------------------------------------------------------------


! -------------------------------------------------------------
! edit here to change the time dependent properties of the 
! selection criteria
! Note, this function is called AFTER the seismogram has been 
! read and filtered.
! -------------------------------------------------------------

subroutine set_up_criteria_arrays_sw_R
  use seismo_variables 

  integer :: i
  double precision :: time
  double precision :: R_vel, R_time, R_time_major_arc
  double precision :: Q_vel, Q_time


! -----------------------------------------------------------------
! Start of user-dependent portion

! This is where you reset the signal_end and noise_end values
! if you want to modify them from the defaults.
! This is also where you modulate the time dependence of the selection
! criteria.  You have access to the following parameters from the 
! seismogram itself:
!
! dt, b, kstnm, knetwk, kcmpnm
! evla, evlo, stla, stlo, evdp, azimuth, backazimuth, dist_deg, dist_km
! num_phases, ph_names, ph_times
!
! Example of modulation:
!-----------------------
! To increase s2n limit after arrival of R1 try
!
! R_vel=3.2
! R_time=dist_km/R_vel
! do i = 1, npts
!   time=b+(i-1)*dt
!   if (time.gt.R_time) then
!     S2N_LIMIT(i)=2*WINDOW_S2N_BASE
!   endif
! enddo
!

 ! --------------------------------
 ! Set approximate end of rayleigh wave arrival
 R_vel=4.0
 R_time=dist_km/R_vel
 R_time_major_arc=(CIRCUM_EARTH-dist_km)/R_vel
 ! --------------------------------
 ! Set approximate start of love wave arrival
 !Q_vel=4.2
 Q_vel=4.5
 Q_time=dist_km/Q_vel

 ! reset the signal_end time to be the end of the Rayleigh waves
! if (DATA_QUALITY) then
!   signal_end=R_time
! endif
  !noise_end  = P_pick - 0.1*P_pick ! vertical and radial comp.
  ! noise_end  = S_pick - 0.2*S_pick ! transverse comp.
  !noise_start = b
  print *, "R, dist_km:",dist_km, trim(kstnm)


 ! --------------------------------
 ! modulate criteria in time
 do i = 1, npts
   time=b+(i-1)*dt
   ! --------------------------------
   ! if we are beyond the Rayleigh wave, then make the all criteria stronger
   ! ratio criterion stronger
  !! if (time.gt.Q_time+WIN_MAX_PERIOD) then
  !! if (time.gt.Q_time) then
    !! S2N_LIMIT(i)=10*WINDOW_S2N_BASE    ! only pick big signals
    !! CC_LIMIT(i)= 0.95                  ! only pick very similar signals
    !! TSHIFT_LIMIT(i)=TSHIFT_BASE/3.0    ! only pick small timeshifts
    !! DLNA_LIMIT(i)=DLNA_BASE/3.0        ! only pick small amplitude anomalies
    !! STALTA_W_LEVEL(i)=STALTA_BASE*2.0     ! pick only distinctive arrivals
   !!endif
   ! --------------------------------
   ! if we are in the surface wave times, then make the cross-correlation
   ! criterion less severe
   !!if (time.gt.Q_time .and. time.lt.R_time) then
   !! CC_LIMIT(i)=0.9*CC_LIMIT(i)
   !!endif
   ! --------------------------------
   ! if we are beyond the Rayleigh wave, then make the all criteria stronger
   ! ratio criterion stronger
  !if (time.gt.(R_time+WIN_MAX_PERIOD)) then
  if( evdp.lt.200.0 ) then
    !shallow event: major-arc surface wave
    if ( time.gt.(R_time+3*WIN_MAX_PERIOD) .and. &
        time.lt. ( R_time_major_arc-3*WIN_MAX_PERIOD ) ) then
   !if (time.gt.R_time) then
      S2N_LIMIT(i)=10*WINDOW_S2N_BASE    ! only pick big signals
      CC_LIMIT(i)= CC_LIMIT(i)+0.05     ! only pick very similar signals
      TSHIFT_LIMIT(i)=TSHIFT_BASE/2      ! only pick small timeshifts
      DLNA_LIMIT(i)=DLNA_BASE/5.0       ! only pick small amplitude anomalies
      !STALTA_W_LEVEL(i)=STALTA_BASE*5 ! pick only distinctive arrivals
      STALTA_W_LEVEL(i)=1 ! pick only distinctive arrivals
    endif

    if ( time.gt.(R_time_major_arc+3*WIN_MAX_PERIOD) ) then
      S2N_LIMIT(i)=3*WINDOW_S2N_BASE    ! only pick big signals
      CC_LIMIT(i)= CC_LIMIT(i)+0.03     ! only pick very similar signals
      !TSHIFT_LIMIT(i)=TSHIFT_BASE      ! only pick small timeshifts
      DLNA_LIMIT(i)=DLNA_BASE/2.0       ! only pick small amplitude anomalies
      STALTA_W_LEVEL(i)=STALTA_BASE*2.5 ! pick only distinctive arrivals
    endif
  else
    !if( time.gt.(R_time+3*WIN_MAX_PERIOD) ) then
    if( time.gt.(R_time) ) then
      S2N_LIMIT(i)=10*WINDOW_S2N_BASE    ! only pick big signals
      CC_LIMIT(i)= 0.95                  ! only pick very similar signals
      TSHIFT_LIMIT(i)=TSHIFT_BASE/2.0    ! only pick small timeshifts
      DLNA_LIMIT(i)=DLNA_BASE/2.0        ! only pick small amplitude anomalies
      STALTA_W_LEVEL(i)=STALTA_BASE*5.0     ! pick only distinctive arrivals
    endif
  endif

   ! --------------------------------
   ! Before Rayleigh waves, then make the all criteria stronger
   ! ratio criterion stronger
  if (time.lt.(R_time-2*WIN_MAX_PERIOD)) then
  ! if (time.gt.R_time) then
    ! S2N_LIMIT(i)=10*WINDOW_S2N_BASE    ! only pick big signals
    ! CC_LIMIT(i)= 0.95                  ! only pick very similar signals
    ! TSHIFT_LIMIT(i)=TSHIFT_BASE/2.0    ! only pick small timeshifts
    ! DLNA_LIMIT(i)=DLNA_BASE/3.0        ! only pick small amplitude anomalies
     STALTA_W_LEVEL(i)=STALTA_BASE*1.8    ! pick only distinctive arrivals
     C1_LIMIT(i)=2.0
  endif
   ! --------------------------------

   ! for source inversion increase S2N ratio
   ! particularly to reject low amplitude signal comparable to noise

   ! reject windows before P or S arrivals
   !if (time.lt.(S_pick - 0.2*S_pick)) then
   if (time.lt.noise_end) then
     STALTA_W_LEVEL(i)=1     ! to avoid windows before P or S arrivals
   endif

    !select only surface waves
   !if (time.lt.Q_time) then
   !  STALTA_W_LEVEL(i)=1
   !endif


   ! --------------------------------
   ! modulate criteria according to event depth
   !
   ! if an intermediate depth event
   !!UNCOMMENT THE LINES BELOW IN CASE OF SOURCE INVERSIONS
!   if (evdp.ge.70 .and. evdp.lt.300) then
!     TSHIFT_LIMIT(i)=TSHIFT_BASE*1.4
!   ! if a deep event
!   elseif (evdp.ge.300) then
!     TSHIFT_LIMIT(i)=TSHIFT_BASE*1.7
!   endif
 enddo
!
! End of user-dependent portion
! -----------------------------------------------------------------
end subroutine
! -------------------------------------------------------------

subroutine set_up_criteria_arrays_sw_T
  use seismo_variables 

  integer :: i
  double precision :: time
  double precision :: R_vel, R_time
  double precision :: Q_vel, Q_time, Q_time_major_arc


! -----------------------------------------------------------------
! Start of user-dependent portion

! This is where you reset the signal_end and noise_end values
! if you want to modify them from the defaults.
! This is also where you modulate the time dependence of the selection
! criteria.  You have access to the following parameters from the 
! seismogram itself:
!
! dt, b, kstnm, knetwk, kcmpnm
! evla, evlo, stla, stlo, evdp, azimuth, backazimuth, dist_deg, dist_km
! num_phases, ph_names, ph_times
!
! Example of modulation:
!-----------------------
! To increase s2n limit after arrival of R1 try
!
! R_vel=3.2
! R_time=dist_km/R_vel
! do i = 1, npts
!   time=b+(i-1)*dt
!   if (time.gt.R_time) then
!     S2N_LIMIT(i)=2*WINDOW_S2N_BASE
!   endif
! enddo
!

 ! --------------------------------
 ! Set approximate end of rayleigh wave arrival
 R_vel=4.0
 R_time=dist_km/R_vel
 ! --------------------------------
 ! Set approximate start of love wave arrival
 !Q_vel=4.2
 Q_vel=4.5
 Q_time=dist_km/Q_vel
 Q_time_major_arc=(CIRCUM_EARTH-dist_km)/Q_vel

 ! reset the signal_end time to be the end of the Rayleigh waves
! if (DATA_QUALITY) then
!   signal_end=R_time
! endif
  !noise_end  = P_pick - 0.1*P_pick ! vertical and radial comp.
  ! noise_end  = S_pick - 0.2*S_pick ! transverse comp.
  !noise_start = b


 ! --------------------------------
 ! modulate criteria in time
 do i = 1, npts
   time=b+(i-1)*dt
   ! --------------------------------
   ! if we are beyond the Rayleigh wave, then make the all criteria stronger
   ! ratio criterion stronger
  !! if (time.gt.Q_time+WIN_MAX_PERIOD) then
  !! if (time.gt.Q_time) then
    !! S2N_LIMIT(i)=10*WINDOW_S2N_BASE    ! only pick big signals
    !! CC_LIMIT(i)= 0.95                  ! only pick very similar signals
    !! TSHIFT_LIMIT(i)=TSHIFT_BASE/3.0    ! only pick small timeshifts
    !! DLNA_LIMIT(i)=DLNA_BASE/3.0        ! only pick small amplitude anomalies
    !! STALTA_W_LEVEL(i)=STALTA_BASE*2.0     ! pick only distinctive arrivals
   !!endif
   ! --------------------------------
   ! if we are in the surface wave times, then make the cross-correlation
   ! criterion less severe
   !!if (time.gt.Q_time .and. time.lt.R_time) then
   !! CC_LIMIT(i)=0.9*CC_LIMIT(i)
   !!endif
   ! --------------------------------
   ! if we are beyond the Rayleigh wave, then make the all criteria stronger
   ! ratio criterion stronger
  !if (time.gt.(R_time+WIN_MAX_PERIOD)) then
  if( evdp.lt.200.0 ) then
    !shallow earthquake
    if ( time.gt.(Q_time+2*WIN_MAX_PERIOD) .and. &
        time.lt. (Q_time_major_arc-3*WIN_MAX_PERIOD) ) then
    !if (time.gt.R_time) then
      S2N_LIMIT(i)=5*WINDOW_S2N_BASE    ! only pick big signals
      CC_LIMIT(i)= CC_LIMIT(i)+0.05     ! only pick very similar signals
      !TSHIFT_LIMIT(i)=TSHIFT_BASE      ! only pick small timeshifts
      DLNA_LIMIT(i)=DLNA_BASE/3.0       ! only pick small amplitude anomalies
      STALTA_W_LEVEL(i)=STALTA_BASE*5   ! pick only distinctive arrivals
    endif

    if ( time.gt.(Q_time_major_arc+3*WIN_MAX_PERIOD) ) then
      S2N_LIMIT(i)=3*WINDOW_S2N_BASE    ! only pick big signals
      CC_LIMIT(i)= CC_LIMIT(i)+0.03     ! only pick very similar signals
      !TSHIFT_LIMIT(i)=TSHIFT_BASE      ! only pick small timeshifts
      DLNA_LIMIT(i)=DLNA_BASE/2.0       ! only pick small amplitude anomalies
      STALTA_W_LEVEL(i)=STALTA_BASE*2.5 ! pick only distinctive arrivals
    endif
  else
    !if( time.gt.(Q_time+2*WIN_MAX_PERIOD)) then
    if( time.gt.(Q_time)) then
      S2N_LIMIT(i)=10*WINDOW_S2N_BASE    ! only pick big signals
      CC_LIMIT(i)= 0.95                  ! only pick very similar signals
      TSHIFT_LIMIT(i)=TSHIFT_BASE/2.0    ! only pick small timeshifts
      DLNA_LIMIT(i)=DLNA_BASE/2.0        ! only pick small amplitude anomalies
      STALTA_W_LEVEL(i)=STALTA_BASE*5.0     ! pick only distinctive arrivals
    endif
  endif
   ! --------------------------------
   ! Before Rayleigh waves, then make the all criteria stronger
   ! ratio criterion stronger
  if (time.lt.(Q_time-2*WIN_MAX_PERIOD)) then
  ! if (time.gt.R_time) then
    ! S2N_LIMIT(i)=10*WINDOW_S2N_BASE    ! only pick big signals
    ! CC_LIMIT(i)= 0.95                  ! only pick very similar signals
    ! TSHIFT_LIMIT(i)=TSHIFT_BASE/2.0    ! only pick small timeshifts
    ! DLNA_LIMIT(i)=DLNA_BASE/3.0        ! only pick small amplitude anomalies
     STALTA_W_LEVEL(i)=STALTA_BASE*1.8   ! pick only distinctive arrivals
     C1_LIMIT(i)=2.0
  endif
   ! --------------------------------

   ! for source inversion increase S2N ratio
   ! particularly to reject low amplitude signal comparable to noise

   ! reject windows before P or S arrivals
   !if (time.lt.(S_pick - 0.2*S_pick)) then
   if (time.lt.noise_end) then
     STALTA_W_LEVEL(i)=1     ! to avoid windows before P or S arrivals
   endif

    !select only surface waves
   !if (time.lt.Q_time) then
   !  STALTA_W_LEVEL(i)=1
   !endif


   ! --------------------------------
   ! modulate criteria according to event depth
   !
   ! if an intermediate depth event
   !!UNCOMMENT THE LINES BELOW IN CASE OF SOURCE INVERSIONS
!   if (evdp.ge.70 .and. evdp.lt.300) then
!     TSHIFT_LIMIT(i)=TSHIFT_BASE*1.4
!   ! if a deep event
!   elseif (evdp.ge.300) then
!     TSHIFT_LIMIT(i)=TSHIFT_BASE*1.7
!   endif
 enddo
!
! End of user-dependent portion
! -----------------------------------------------------------------
end subroutine

! -------------------------------------------------------------
! edit here to change the time dependent properties of the 
! selection criteria
! Note, this function is called AFTER the seismogram has been 
! read and filtered.
! -------------------------------------------------------------

subroutine set_up_criteria_arrays_sw_Z
  use seismo_variables 

  integer :: i
  double precision :: time
  double precision :: R_vel, R_time, R_time_major_arc
  double precision :: Q_vel, Q_time


! -----------------------------------------------------------------
! Start of user-dependent portion

! This is where you reset the signal_end and noise_end values
! if you want to modify them from the defaults.
! This is also where you modulate the time dependence of the selection
! criteria.  You have access to the following parameters from the 
! seismogram itself:
!
! dt, b, kstnm, knetwk, kcmpnm
! evla, evlo, stla, stlo, evdp, azimuth, backazimuth, dist_deg, dist_km
! num_phases, ph_names, ph_times
!
! Example of modulation:
!-----------------------
! To increase s2n limit after arrival of R1 try
!
! R_vel=3.2
! R_time=dist_km/R_vel
! do i = 1, npts
!   time=b+(i-1)*dt
!   if (time.gt.R_time) then
!     S2N_LIMIT(i)=2*WINDOW_S2N_BASE
!   endif
! enddo
!

 ! --------------------------------
 ! Set approximate end of rayleigh wave arrival
 !R_vel=3.8
 R_vel=4.0
 R_time=dist_km/R_vel
 R_time_major_arc=(CIRCUM_EARTH-dist_km)/R_vel
 print *, "R_time and R_time_ac:", R_time, R_time_major_arc
 ! --------------------------------
 ! Set approximate start of love wave arrival
! Q_vel=4.0
 Q_vel=4.5
 Q_time=dist_km/Q_vel

 ! reset the signal_end time to be the end of the Rayleigh waves
 !if (DATA_QUALITY) then
 !  signal_end=R_time
 !endif
  !noise_end  = P_pick - 0.1*P_pick ! vertical and radial comp.
  !noise_end  = S_pick - 0.2*S_pick ! transverse comp.
  ! noise_start = b


 ! --------------------------------
 ! modulate criteria in time
 do i = 1, npts
   time=b+(i-1)*dt
   ! --------------------------------
   ! if we are beyond the Rayleigh wave, then make the all criteria stronger
   ! ratio criterion stronger
  !if (time.gt.(R_time+WIN_MAX_PERIOD)) then
  if( evdp.lt.200.0 )then
    if ( time.gt.(R_time+3*WIN_MAX_PERIOD) .and. &
        time.lt. ( R_time_major_arc-3*WIN_MAX_PERIOD ) ) then
    !if (time.gt.R_time) then
      S2N_LIMIT(i)=3*WINDOW_S2N_BASE    ! only pick big signals
      CC_LIMIT(i)= CC_LIMIT(i)+0.03     ! only pick very similar signals
      !TSHIFT_LIMIT(i)=TSHIFT_BASE      ! only pick small timeshifts
      DLNA_LIMIT(i)=DLNA_BASE/2.0       ! only pick small amplitude anomalies
      STALTA_W_LEVEL(i)=STALTA_BASE*2.5 ! pick only distinctive arrivals
    endif

    if ( time.gt.R_time_major_arc+3*WIN_MAX_PERIOD  ) then
      S2N_LIMIT(i)=3*WINDOW_S2N_BASE    ! only pick big signals
      CC_LIMIT(i)= CC_LIMIT(i)+0.03     ! only pick very similar signals
      !TSHIFT_LIMIT(i)=TSHIFT_BASE      ! only pick small timeshifts
      DLNA_LIMIT(i)=DLNA_BASE/2.0       ! only pick small amplitude anomalies
      STALTA_W_LEVEL(i)=STALTA_BASE*2.5 ! pick only distinctive arrivals
    endif
  else
    !if( time.gt.(R_time+3*WIN_MAX_PERIOD)) then
    if( time.gt.(R_time)) then
      S2N_LIMIT(i)=10*WINDOW_S2N_BASE    ! only pick big signals
      CC_LIMIT(i)= 0.95                  ! only pick very similar signals
      TSHIFT_LIMIT(i)=TSHIFT_BASE/2.0    ! only pick small timeshifts
      DLNA_LIMIT(i)=DLNA_BASE/2.0        ! only pick small amplitude anomalies
      STALTA_W_LEVEL(i)=STALTA_BASE*5.0     ! pick only distinctive arrivals
    endif
  endif
  !if (time.gt.(dist_km/3.7+2*WIN_MAX_PERIOD)) then
   !if (time.gt.R_time) then
  !   S2N_LIMIT(i)=10*WINDOW_S2N_BASE    ! only pick big signals
  !   CC_LIMIT(i)= 0.95                  ! only pick very similar signals
  !   TSHIFT_LIMIT(i)=TSHIFT_BASE        ! only pick small timeshifts
  !   DLNA_LIMIT(i)=DLNA_BASE/3.0        ! only pick small amplitude anomalies
  !   STALTA_W_LEVEL(i)=STALTA_BASE*2.0     ! pick only distinctive arrivals

  ! endif
   ! --------------------------------
   ! Before Rayleigh waves, then make the all criteria stronger
   ! ratio criterion stronger
  if (time.lt.(R_time-2*WIN_MAX_PERIOD)) then
  ! if (time.gt.R_time) then
    ! S2N_LIMIT(i)=WINDOW_S2N_BASE    ! only pick big signals
    ! CC_LIMIT(i)= 0.95                  ! only pick very similar signals
     !TSHIFT_LIMIT(i)=TSHIFT_BASE/2.0    ! only pick small timeshifts
     !DLNA_LIMIT(i)=DLNA_BASE/2.0        ! only pick small amplitude anomalies
     STALTA_W_LEVEL(i)=STALTA_BASE*1.8   ! pick only distinctive arrivals
     C1_LIMIT(i)=2.0
   endif
   ! --------------------------------

   ! if we are in the surface wave times, then make the cross-correlation
   ! criterion less severe
   !!if (time.gt.Q_time .and. time.lt.R_time) then
   !! CC_LIMIT(i)=0.9*CC_LIMIT(i)
   !!endif

   ! for source inversion increase S2N ratio
   ! particularly to reject low amplitude signal comparable to noise

   ! reject windows before P or S arrivals
   if (time.lt.noise_end) then
     STALTA_W_LEVEL(i)=1     ! to avoid windows before P or S arrivals
   endif

   !select only surface waves
   !if (time.lt.R_time) then
   !  STALTA_W_LEVEL(i)=1
   !endif
  
   ! --------------------------------
   ! modulate criteria according to event depth
   !
   !! UNCOMMENT THE LINES BELOW IN CASE OF SOURCE INVERSIONS!!
   ! if an intermediate depth event
!   if (evdp.ge.70 .and. evdp.lt.300) then
!     TSHIFT_LIMIT(i)=TSHIFT_BASE*1.4
   !! if a deep event
!   elseif (evdp.ge.300) then
!     TSHIFT_LIMIT(i)=TSHIFT_BASE*1.7
!   endif
 enddo

!
! End of user-dependent portion
! -----------------------------------------------------------------
end subroutine
! -------------------------------------------------------------


! -------------------------------------------------------------
! edit here to change the time dependent properties of the 
! selection criteria
! Note, this function is called AFTER the seismogram has been 
! read and filtered.
! -------------------------------------------------------------
subroutine set_up_criteria_arrays_default
  use seismo_variables 

  integer :: i
  double precision :: time
  double precision :: R_vel, R_time
  double precision :: Q_vel, Q_time

! -----------------------------------------------------------------
! This is the basic version of the subroutine - no variation with time
! -----------------------------------------------------------------
  do i = 1, npts
    time=b+(i-1)*dt
    DLNA_LIMIT(i)=DLNA_BASE
    CC_LIMIT(i)=CC_BASE
    TSHIFT_LIMIT(i)=TSHIFT_BASE
    STALTA_W_LEVEL(i)=STALTA_BASE
    S2N_LIMIT(i)=WINDOW_S2N_BASE
    C1_LIMIT(i)=C_1
  enddo

  ! these values will be used for signal2noise calculations
  ! if DATA_QUALITY=.true.
  if (DATA_QUALITY) then
    !noise_end=ph_times(1)-WIN_MAX_PERIOD
    noise_end=ph_times(1)-WIN_MAX_PERIOD/2
    noise_start=b
    signal_end=b+(npts-1)*dt
    signal_start = noise_end
  endif

!
! End of user-dependent portion
! -----------------------------------------------------------------
end subroutine
! -------------------------------------------------------------


subroutine modify_water_level_arrays_RSS

  use seismo_variables

  integer :: i
  double precision :: time
  double precision :: R_time, Q_time
  double precision :: R_vel, Q_vel

  R_vel=4.0
  R_time=dist_km/R_vel
  !R_time_major_arc=(CIRCUM_EARTH-dist_km)/R_vel
  !print *, "R_time and R_time_ac:", R_time, R_time_major_arc
  ! --------------------------------
  ! Set approximate start of love wave arrival
  ! Q_vel=4.0
  Q_vel=4.5
  Q_time=dist_km/Q_vel

  if( kcmpnm(3:3).eq."Z" .or. kcmpnm(3:3).eq.'R' )then
    time_zone=R_time
  else if( kcmpnm(3:3).eq."T" )then
    time_zone=Q_time
  endif
  if(FOCUS_PART.eq.1)then
  !FOCUS_PART==1, focus on body wave and surface wave
  !set the stalat_limit to 1 after surface wave to reject those windows
    do i = 1, npts
      time=b+(i-1)*dt
      if(time.gt.time_zone+5*MAX_WIN_PERIOD)then
        STALTA_W_LEVEL(i)=1.0
      endif
    enddo
  else if(FOCUS_PART.eq.2)then
  !FOCUS_PART==2, focus on phases after surface wave
  !reject windows before that
    do i = 1, npts
      time=b+(i-1)*dt
      if(time.lt.time_zone+5*MAX_WIN_PERIOD)then
        STALTA_W_LEVEL(i)=1.0
      endif
    enddo
  endif

end subroutine modify_water_level_arrays_RSS

subroutine modify_lp_env_RSS

  use seismo_variables

  integer :: i
  double precision :: time_zone
  double precision :: R_time, Q_time
  double precision :: R_vel, Q_vel
  double precision :: damp_factor

  R_vel=4.0
  R_time=dist_km/R_vel
  Q_vel=4.5
  Q_time=dist_km/Q_vel

  if( kcmpnm(3:3).eq."Z" .or. kcmpnm(3:3).eq.'R' )then
    time_zone=R_time
  elseif( kcmpnm(3:3).eq."T" )then
    time_zone=Q_time
  endif

  do i=1, npts
    time=b+(i-1)*dt
    if( time.lt.time_zone+3*MAX_WIN_PERIOD)then
      env_synt_lp(i)=0.0
    elseif( (time.gt.time_zone+3*MAX_WIN_PERIOD) .and. &
        (time.lt.time_zone+5*MAX_WIN_PERIOD) )then
        damp_factor=(time-time_zone-3*MAX_WIN_PERIOD)/(2*MAX_WIN_PERIOD)
        env_synt_lp(i)=evn_synt_lp*damp_factor
    endif
  enddo

end subroutine modify_lp_env_RSS

