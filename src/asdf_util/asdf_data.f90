module asdf_data

  !max number of records on one processor
  integer, parameter :: MAXDATA_PER_PROC = 10000
  !max number of records in one asdf file
  integer, parameter :: MAXDATA_TOTAL = 30000

  type asdf_record
    real(kind=8), allocatable :: record(:)
  end type asdf_record

  type asdf_response
    character(len=500000) :: response_string
    integer :: response_length
  end type asdf_response

  type asdf_event
    ! scalars
    character(len=13)     :: event
    real(kind=8), allocatable     :: event_lat(:), event_lo(:), event_dpt(:)

    !size info
    integer           :: nrecords
    integer           :: nreceivers

    !Processing info
    real(kind=8)              :: min_period, max_period

    !time info
    integer, allocatable    :: gmt_year(:), gmt_day(:), gmt_hour(:)
    integer, allocatable    :: gmt_min(:), gmt_sec(:), gmt_msec(:)

    !seismic record info
    integer, allocatable    :: npoints(:)
    real(kind=8), allocatable       :: receiver_lat(:), receiver_lo(:)
    real(kind=8), allocatable       :: receiver_el(:),  receiver_dpt(:)
    real(kind=8), allocatable       :: begin_value(:),  end_value(:)
    real(kind=8), allocatable       :: cmp_azimuth(:),  cmp_incident_ang(:)
    real(kind=8), allocatable       :: sample_rate(:),  scale_factor(:)

    real(kind=8), allocatable       :: ev_to_sta_AZ(:), sta_to_ev_AZ(:)
    real(kind=8), allocatable       :: great_circle_arc(:) 
    real(kind=8), allocatable       :: dist(:)
    real(kind=8), allocatable       :: P_pick(:), S_pick(:)

    character(len=20),allocatable :: receiver_name_array(:), network_array(:)
    character(len=20),allocatable :: component_array(:), receiver_id_array(:)

    !seismograms
    type (asdf_record), allocatable :: records(:)
    
    !instrument response
    logical :: STORE_RESPONSE
    type (asdf_response), allocatable :: responses(:)

  end type asdf_event

end module asdf_data
