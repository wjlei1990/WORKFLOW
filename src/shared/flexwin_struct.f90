module flexwin_struct

  type win_info
    !window output
    integer :: num_win
    double precision,allocatable :: t_start(:)
    double precision,allocatable :: t_end(:)
  end type win_info

  !type win_chi_info
  !    !window chi information
  !    integer :: num_win         !row
  !    integer :: num_measurement !column
  !    double precision, allocatable :: chi(:,:) 
  !end type win_chi_info

  type flexwin_par_struct
		sequence
    logical :: DEBUG
    logical :: MAKE_SEISMO_PLOTS
    logical :: MAKE_WINDOW_FILES
    logical :: BODY_WAVE_ONLY
    logical :: RUN_BANDPASS
    logical :: DATA_QUALITY
    double precision :: WIN_MIN_PERIOD, WIN_MAX_PERIOD
    double precision :: FSTART, FEND
    double precision :: STALTA_BASE
    double precision :: TSHIFT_BASE, TSHIFT_REFERENCE
    double precision :: DLNA_BASE, DLNA_REFERENCE
    double precision :: CC_BASE
    double precision :: SNR_INTEGRATE_BASE, SNR_MAX_BASE
    double precision :: WINDOW_S2N_BASE
    double precision :: C_0,C_1,C_2,C_3a,C_3b,C_4a,C_4b
    double precision :: WEIGHT_SPACE_COVERAGE
    double precision :: WEIGHT_AVERAGE_CC
    double precision :: WEIGHT_N_WINDOWS
  end type flexwin_par_struct

  type flexwin_par_struct_all
		sequence
    type(flexwin_par_struct) :: T
    type(flexwin_par_struct) :: R
    type(flexwin_par_struct) :: Z
  end type flexwin_par_struct_all

end module flexwin_struct
