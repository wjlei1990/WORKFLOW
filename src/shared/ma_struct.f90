module ma_struct

  !type win_info
    !window output
  !  integer :: num_win
  !  double precision,allocatable :: t_start(:)
  !  double precision,allocatable :: t_end(:)
  !end type win_info

  type win_chi_info
      !window chi information
      !inside it is "window_chi" txt file, which is used to keep all the chi
      !info inside the file
      integer :: num_win         !row
      integer :: num_measurement !column
      double precision, allocatable :: chi(:,:) !size(num_win,num_measure) 
      !still got questions here???
      !double precision, allocatable :: all_chi(:)
      integer, allocatable :: imeas(:)
      double precision,allocatable :: tr_chi(:)
      double precision,allocatable :: am_chi(:)
      double precision,allocatable :: T_pmax_dat(:), T_pmax_syn(:)
  end type win_chi_info

  type ma_par_struct
    sequence
    logical :: RUN_BANDPASS
    logical :: DISPLAY_DETAILS
    logical :: OUTPUT_MEASUREMENT_FILES
    logical :: COMPUTE_ADJOINT_SOURCE
    logical :: USE_PHYSICAL_DISPERSION
    integer :: nn
    integer :: imeas0
    integer :: is_mtm0
    integer :: ERROR_TYPE
    integer :: ITAPER
    double precision :: tt, dtt
    double precision :: TLONG, TSHORT
    double precision :: fstart0, fend0
    double precision :: TSHIFT_MIN, TSHIFT_MAX
    double precision :: DLNA_MIN, DLNA_MAX
    double precision :: CC_MIN
    double precision :: DT_SIGMA_MIN, DLNA_SIGMA_MIN
    double precision :: WTR, NPI
    double precision :: DT_FAC, ERR_FAC, DT_MAX_SCALE, NCYCLE_IN_WINDOW
    character(len=8) :: chan
  end type ma_par_struct
    
  type ma_par_struct_all
    sequence
    type(ma_par_struct) :: R
    type(ma_par_struct) :: T
    type(ma_par_struct) :: Z
  end type ma_par_struct_all

  type ma_weighting_par_struct
    double precision :: weight_T, weight_R, weight_Z
    double precision :: num_P_SV_V, num_P_SV_R, num_SH_T
    double precision :: num_Rayleigh_V, num_Rayleigh_R, num_Love_T
  end type ma_weighting_par_struct

end module ma_struct
