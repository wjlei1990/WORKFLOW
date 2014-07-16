
  module process_var
   
    use process_par

    integer, parameter :: MAX_TRACE_LENGTH=1000000

    ! Directories and Files
    character(len=MAX_FILE_LENGTH) :: DATADIR, OUTDIR, OBSD_SAC, SYNT_SAC
    character(len=MAX_FILE_LENGTH) :: obsd_proc_file, synt_proc_file, CMT_FILE
    logical                        :: file_exists

    ! Values read from the Parameter file
    logical                 :: DEBUG, RESP_FLAG, PZs_FLAG, DISP, VEL
    !double precision        :: MIN_PERIOD, MAX_PERIOD

    ! The event name
    !character(len=13)       :: event_name

    ! Used to read in record in the station file
    character(len=10),dimension(MAX_STATIONS) :: sta_name, nw_name
    double precision, dimension(MAX_STATIONS) :: stlat, stlon, stele, stbur
    
    ! Used to return the station, network, component, receiver id
    character(len=20) :: station(MAX_STATIONS), network(MAX_STATIONS)
    character(len=20) :: component(MAX_STATIONS), receiver_id(MAX_STATIONS)

    ! Timing information
    integer                 :: gmt_year, gmt_day, gmt_hour, gmt_min, gmt_sec, gmt_msec 
    integer                 :: yr, mo, da, ho, mi
    real(kind=8)            :: sec, delta_t
    !real(kind=8)            :: sec, origin_time, begin_time, t_shift

    ! CMT Event information
    real(kind=8)            :: cmt_lat, cmt_lon, cmt_depth, cmt_hdur

    ! Number of points in the time-series
    integer                 :: npoints_observed, npoints_synthetic

    ! Number of receivers, records
    integer                 :: ireceiver, irecord, irecord_local
    integer                 :: nreceivers, nrecords

    ! Used for cutting observed and synthetics
    !integer                   :: cuterr, cut_length, npoints_start, npoints_end, nfillb, nfille
    !integer                   :: npoints_cut_observed, npoints_cut_synthetic
    !real                      :: begin_max, end_min, delta_d, delta_s
    !real, allocatable         :: cut_observed(:), cut_synthetic(:)
    !real, allocatable         :: observed_final(:), synthetic_final(:)
    !real(kind=8), allocatable :: time_observed(:), time_synthetic(:)

    ! Used for deconvolution
    !integer :: LNPT, FFT_NPTS
    !integer :: ipts
    !real :: mean, yint, slope, t
    real(kind=8), allocatable :: observed_long(:), synthetic_long(:), time_long_observed(:), time_long_synthetic(:)

    ! Used for interpolating seismograms
    real :: error
    real(kind=8), allocatable :: time_observed_final(:), time_synthetic_final(:)

  end module process_var
