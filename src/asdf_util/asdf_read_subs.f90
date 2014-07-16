module asdf_read_subs

!> The module asdf_subs contains Parallel ASDF I/O API:
!! 1)define_asdf_data
!! 2)read_asdf_file
!! 3)write_asdf_file
  use asdf_data
  implicit none

  type sta_info
    character(len=20) :: sta, nw, comp, rid
    integer :: loc
  end type sta_info

  !max number of records on one processor(used in allocated station information in
  !main program)
  !integer, parameter :: MAXDATA_PER_PROC = 10000
  !max number of records total(used to read out the string of station info)
  !integer, parameter :: MAXDATA_TOTAL = 20000

  !include "asdf_constants.h"

contains

subroutine read_asdf_file(file_name, my_asdf, nrecords, &
    station, network, component, receiver_id, option, &
    rank, nproc, comm, ierr)
  
  !use asdf_data

  character(len=100),intent(in) :: file_name
  type(asdf_event), intent(inout) :: my_asdf
  integer :: nrecords, option
  character(len=*) :: station(:), network(:), component(:), receiver_id(:)
  integer,intent(in) :: rank, nproc, comm
  integer :: ierr
  !logical,intent(in) :: resp_flag

  if(rank.eq.0) print *, "Reading file begin: ", trim(file_name)

  if(option.eq.0) then
    !option=0, read the data and return the sta, nw, comp and rid
    call read_asdf_file_0(file_name, my_asdf, nrecords, &
      station, network, component, receiver_id, &
      rank, nproc, comm, ierr)
  elseif(option.eq.1)then
    !option=1, read the data based on sta, nw and comp, return rid
    call read_asdf_file_1(file_name, my_asdf, nrecords, &
      station, network, component, receiver_id, &
      rank, nproc, comm, ierr)
  elseif(option.eq.2)then
    !option=2, read the whole file
    call read_asdf_file_2(file_name, my_asdf, nrecords, &
      station, network, component, receiver_id, &
      rank, nproc, comm, ierr)
  else
    print *, "check the option value: 0, 1 or 2"
  endif

  call MPI_Barrier(comm, ierr)
  if(rank.eq.0) print *, "Reading file finished: ", trim(file_name)
  
end subroutine read_asdf_file

!! \param file_name The name of the asdf file to read
!! \param my_asdf The asdf object that will be read
!! \param comm Size of the group associated with the MPI communicator

subroutine read_asdf_file_0(file_name, my_asdf, nrecords, &
    sta, nw, comp, rid, &
    rank, nproc, comm, ierr)

  use adios_read_mod
  !use asdf_data
  implicit none

  character(len=100),intent(in) :: file_name
  type(asdf_event), intent(inout) :: my_asdf
  integer :: nrecords
  character(len=*) :: sta(:), nw(:), comp(:), rid(:)
  integer,intent(in) :: rank, nproc, comm
  integer :: ierr, errorcode
  !logical,intent(in) :: resp_flag
  logical :: resp_flag
  integer :: resp_flag_int

  integer                 :: i, j, scalarnum, local_index
  integer                 :: vcnt, acnt, tfirst, tlast
  integer                 :: vrank, vtype, vsteps, adios_err

  integer(kind=8)         :: fh, sel=0
  integer(kind=8),dimension(1)   :: start, count
  integer(kind=8),dimension(10)  :: readsize, dims
  integer :: dim_array
  integer :: nrecords_total, nrecords_local
  integer :: loc_begin, loc_end

  character(len=128),dimension(:),allocatable :: vnamelist

  character(len=20), allocatable :: receiver_name_array_temp(:)
  character(len=20), allocatable :: network_array_temp(:)
  character(len=20), allocatable :: component_array_temp(:)
  character(len=20), allocatable :: receiver_id_array_temp(:)

  character(len=30) :: loc_string
  integer :: receiver_name_len, network_len, component_len, receiver_id_len
  character(len=:), allocatable :: receiver_name, network
  character(len=:), allocatable :: component, receiver_id 

  !>Initialization,Get Varname and Varnumber
  call adios_read_init_method (ADIOS_READ_METHOD_BP, comm, "verbose=2", ierr)
  call adios_read_open_file (fh, trim(adjustl(file_name)), 0, comm, ierr)
  call adios_inq_file (fh, vcnt, acnt, tfirst, tlast, ierr)

  !get the number of records in the asdf file
  call adios_get_scalar (fh,"/nrecords",nrecords_total,ierr)
  call adios_get_scalar (fh,"/STORE_RESPONSE", resp_flag_int, ierr )
  !change the resp_flag_int(integer) into resp_flag(logical)
  if(resp_flag_int.eq.1)then
    resp_flag=.true.
  else
    resp_flag=.false.
  endif

  if(rank==0)then
    print *,"nrecords_total:", nrecords_total
    if(nrecords_total.gt.MAXDATA_TOTAL)then
      print *, "Increase the MAXDATA_TOTAL"
      call MPI_ABORT(comm, errorcode, ierr)
    endif
  endif

  allocate(receiver_name_array_temp(nrecords_total))
  allocate(network_array_temp(nrecords_total))
  allocate(component_array_temp(nrecords_total))
  allocate(receiver_id_array_temp(nrecords_total))
  
  !read the receiver, network, component and receiver_id string 
  call adios_get_scalar (fh,"/receiver_name_len",receiver_name_len,ierr)
  call adios_get_scalar (fh,"/network_len",network_len,ierr)
  call adios_get_scalar (fh,"/component_len",component_len,ierr)
  call adios_get_scalar (fh,"/receiver_id_len",receiver_id_len,ierr)
  allocate(character(len=6*MAXDATA_TOTAL) :: receiver_name)
  allocate(character(len=6*MAXDATA_TOTAL) :: network)
  allocate(character(len=6*MAXDATA_TOTAL) :: component)
  allocate(character(len=6*MAXDATA_TOTAL) :: receiver_id)
  call adios_get_scalar (fh, "/receiver_name", receiver_name, ierr)
  call adios_get_scalar (fh, "/network", network, ierr)
  call adios_get_scalar (fh, "/component", component, ierr)
  call adios_get_scalar (fh, "/receiver_id", receiver_id, ierr)

  !split the job based on the receiver name(keep 3 components on the same
  !processor)
  call split_job_mpi_complex(nrecords_total,receiver_name, receiver_name_len, &
            nrecords_local, loc_begin, loc_end, rank, nproc)
  print *,"rank, nproc, loc_begin,loc_end,nrecords_local:", &
               rank, nproc, loc_begin, loc_end, nrecords_local
  nrecords=nrecords_local

  if(nrecords.gt.MAXDATA_PER_PROC) then
    print *,"nrecords exceed MAXDATA_PER_PROC. Modify MAXDATA_PER_PROC."
    call MPI_ABORT(comm, errorcode, ierr) 
  endif
  !>allocate variables
  call init_asdf_data(my_asdf, nrecords, resp_flag)

  !split the string and get string array
  call split_string(receiver_name,receiver_name_len, &
                            receiver_name_array_temp,dim_array,'.')
  call split_string(network,network_len, &
                            network_array_temp,dim_array,'.')
  call split_string(component,component_len,&
                            component_array_temp,dim_array,'.')
  call split_string(receiver_id,receiver_id_len,&
                            receiver_id_array_temp,dim_array,'.')

  !get the right receiver_name, network, component, and receiver_id
  my_asdf%receiver_name_array(1:nrecords_local) = &
                          receiver_name_array_temp(loc_begin:loc_end)
  my_asdf%network_array(1:nrecords_local) = &
                          network_array_temp(loc_begin:loc_end)
  my_asdf%component_array(1:nrecords_local) = &
                          component_array_temp(loc_begin:loc_end)
  my_asdf%receiver_id_array(1:nrecords_local) = &
                          receiver_id_array_temp(loc_begin:loc_end)

  !-----------------------------------------------
  !>read all the records 
  do i=1, nrecords
    loc_string=trim(my_asdf%receiver_name_array(i))//"."//&
               trim(my_asdf%network_array(i))//"."//&
               trim(my_asdf%component_array(i))//"."//&
               trim(my_asdf%receiver_id_array(i))
    !get dim info
    call adios_get_scalar(fh, trim(loc_string)//"/global_dim",dims(1), ierr)
    allocate (my_asdf%records(i)%record(dims(1)))
    start(1) = 0
    count(1) = dims(1)
    call adios_selection_boundingbox (sel, 1 , start , count )
    call adios_schedule_read (fh, sel, trim(loc_string)//"/array", 0, 1, &
                               my_asdf%records(i)%record, ierr)
    ! Read in instrument response
    if(my_asdf%STORE_RESPONSE) then
      loc_string="RESP."//trim(my_asdf%network_array(i))//"."//&
                 trim(my_asdf%receiver_name_array(i))//"."//&
                 trim(my_asdf%receiver_id_array(i))//"."//&
                 trim(my_asdf%component_array(i))
      call adios_get_scalar(fh, trim(loc_string)//"/global_dim", dims(1), ierr)
      my_asdf%responses(i)%response_length = dims(1)
      start(1) = 0
      count(1) = dims(1)
      call adios_selection_boundingbox (sel, 1 , start , count )
      call adios_schedule_read (fh, sel, trim(loc_string)//"/array", 0, 1, &
                               my_asdf%responses(i)%response_string, ierr)
    endif
  enddo

  print *,"reading records finished!"

  !--------------------------------------
  !>read in earthquake information
  call adios_get_scalar (fh, "/event", my_asdf%event, ierr)  
  call adios_get_scalar (fh, "/nreceivers", my_asdf%nreceivers, ierr)
  call adios_get_scalar (fh, "/max_period", my_asdf%max_period, ierr)
  call adios_get_scalar (fh, "/min_period", my_asdf%min_period, ierr)
 
  if(rank.eq.0)then
    print *, "my_asdf%event:",my_asdf%event
  endif

  !read in array
  start(1) = loc_begin-1 
  count(1) = my_asdf%nrecords
  call adios_selection_boundingbox (sel, 1 , start , count )
  call adios_schedule_read (fh, sel, "npoints/array", 0, 1, my_asdf%npoints, ierr)

  call adios_schedule_read (fh, sel, "gmt_year/array", 0, 1, my_asdf%gmt_year, ierr)
  call adios_schedule_read (fh, sel, "gmt_day/array", 0, 1, my_asdf%gmt_day, ierr)
  call adios_schedule_read (fh, sel, "gmt_hour/array", 0, 1, my_asdf%gmt_hour, ierr)
  call adios_schedule_read (fh, sel, "gmt_min/array", 0, 1, my_asdf%gmt_min, ierr)
  call adios_schedule_read (fh, sel, "gmt_sec/array", 0, 1, my_asdf%gmt_sec, ierr)
  call adios_schedule_read (fh, sel, "gmt_msec/array", 0, 1, my_asdf%gmt_msec, ierr)

  call adios_schedule_read (fh, sel, "event_lat/array", 0, 1, my_asdf%event_lat, ierr)
  call adios_schedule_read (fh, sel, "event_lo/array", 0, 1, my_asdf%event_lo, ierr)
  call adios_schedule_read (fh, sel, "event_dpt/array", 0, 1, my_asdf%event_dpt, ierr)

  call adios_schedule_read (fh, sel, "receiver_lat/array", 0, 1, my_asdf%receiver_lat, ierr)
  call adios_schedule_read (fh, sel, "receiver_lo/array", 0, 1, my_asdf%receiver_lo, ierr)
  call adios_schedule_read (fh, sel, "receiver_el/array", 0, 1, my_asdf%receiver_el, ierr)
  call adios_schedule_read (fh, sel, "receiver_dpt/array", 0, 1, my_asdf%receiver_dpt, ierr)

  call adios_schedule_read (fh, sel, "begin_value/array", 0, 1, my_asdf%begin_value, ierr)
  call adios_schedule_read (fh, sel, "end_value/array", 0, 1, my_asdf%end_value, ierr)
  call adios_schedule_read (fh, sel, "cmp_azimuth/array", 0, 1, my_asdf%cmp_azimuth, ierr)
  call adios_schedule_read (fh, sel, "cmp_incident_ang/array", 0, 1, my_asdf%cmp_incident_ang, ierr)
  call adios_schedule_read (fh, sel, "sample_rate/array", 0, 1, my_asdf%sample_rate, ierr)
  call adios_schedule_read (fh, sel, "scale_factor/array", 0, 1, my_asdf%scale_factor, ierr)
  call adios_schedule_read (fh, sel, "ev_to_sta_AZ/array", 0, 1, my_asdf%ev_to_sta_AZ, ierr)
  call adios_schedule_read (fh, sel, "sta_to_ev_AZ/array", 0, 1, my_asdf%sta_to_ev_AZ, ierr)
  call adios_schedule_read (fh, sel, "great_circle_arc/array", 0, 1, my_asdf%great_circle_arc, ierr)
  call adios_schedule_read (fh, sel, "dist/array", 0, 1, my_asdf%dist, ierr)
  call adios_schedule_read (fh, sel, "P_pick/array", 0, 1, my_asdf%P_pick, ierr)
  call adios_schedule_read (fh, sel, "S_pick/array", 0, 1, my_asdf%S_pick, ierr)

  !perform the read
  call adios_perform_reads (fh, ierr)
  call adios_read_close(fh, ierr)
  call adios_read_finalize_method(ADIOS_READ_METHOD_BP, adios_err)

  !!return the sta, nw, comp, rid string array
  sta(1:my_asdf%nrecords)=my_asdf%receiver_name_array(1:my_asdf%nrecords)
  nw(1:my_asdf%nrecords)=my_asdf%network_array(1:my_asdf%nrecords)
  comp(1:my_asdf%nrecords)=my_asdf%component_array(1:my_asdf%nrecords)
  rid(1:my_asdf%nrecords)=my_asdf%receiver_id_array(1:my_asdf%nrecords)

end subroutine read_asdf_file_0

subroutine read_asdf_file_1 (file_name, my_asdf, nrecords, &
    sta, nw, comp, rid, &
    rank, nproc, comm, ierr)

  use adios_read_mod
  !use asdf_data
  implicit none

  character(len=100),intent(in) :: file_name
  integer :: nrecords
  character(len=*) :: sta(:), nw(:), comp(:), rid(:)
  type(asdf_event), intent(inout) :: my_asdf
  integer,intent(in) :: rank, nproc, comm
  integer :: ierr, errorcode
  !logical,intent(in) :: resp_flag
  logical :: resp_flag
  integer :: resp_flag_int

  integer                 :: i, j, scalarnum, local_index
  integer                 :: vcnt, acnt, tfirst, tlast
  integer                 :: vrank, vtype, vsteps, adios_err

  integer(kind=8)         :: fh, sel=0, sel_record=0
  integer(kind=8),dimension(1)   :: start, count
  integer(kind=8),dimension(10)  :: readsize, dims
  integer(kind=8) :: npoints_read
  integer(kind=8), dimension(nrecords) :: points_array, points_array_temp
  integer :: dim_array
  integer :: nrecords_total, nrecords_local
  integer :: loc_begin, loc_end

  character(len=128),dimension(:),allocatable :: vnamelist

  character(len=20), allocatable :: receiver_name_array_temp(:)
  character(len=20), allocatable :: network_array_temp(:)
  character(len=20), allocatable :: component_array_temp(:)
  character(len=20), allocatable :: receiver_id_array_temp(:)

  character(len=30) :: loc_string
  integer :: receiver_name_len, network_len, component_len, receiver_id_len
  character(len=:), allocatable :: receiver_name, network
  character(len=:), allocatable :: component, receiver_id 


  real,dimension(20000)::temp_record


  !>Initialization,Get Varname and Varnumber
  call adios_read_init_method (ADIOS_READ_METHOD_BP, comm, "verbose=2", ierr)
  !print *,"filename",trim(adjustl(file_name))
  call adios_read_open_file (fh, trim(adjustl(file_name)), 0, comm, ierr)
  call adios_inq_file (fh, vcnt, acnt, tfirst, tlast, ierr)


  !>get the number of records and npts
  call adios_get_scalar (fh,"/nrecords",nrecords_total,ierr)
  call adios_get_scalar (fh,"/STORE_RESPONSE",resp_flag_int,ierr)
  if(resp_flag_int.eq.1)then
    resp_flag=.true.
  else
    resp_flag=.false.
  endif
  !print *, "resp_flag: ",resp_flag

  if(rank==0)then
    print *,"nrecords_total:", nrecords_total
    if(nrecords_total.gt.MAXDATA_TOTAL)then
      print *, "nrecords_total is larger than MAXDATA_TOTAL"
      call MPI_ABORT(comm, errorcode, ierr)
    endif
  endif

  allocate(receiver_name_array_temp(nrecords_total))
  allocate(network_array_temp(nrecords_total))
  allocate(component_array_temp(nrecords_total))
  allocate(receiver_id_array_temp(nrecords_total))

  call adios_get_scalar (fh,"/receiver_name_len",receiver_name_len,ierr)
  call adios_get_scalar (fh,"/network_len",network_len,ierr)
  call adios_get_scalar (fh,"/component_len",component_len,ierr)
  call adios_get_scalar (fh,"/receiver_id_len",receiver_id_len,ierr)
  allocate(character(len=6*MAXDATA_TOTAL) :: receiver_name)
  allocate(character(len=6*MAXDATA_TOTAL) :: network)
  allocate(character(len=6*MAXDATA_TOTAL) :: component)
  allocate(character(len=6*MAXDATA_TOTAL) :: receiver_id)
  call adios_get_scalar (fh, "/receiver_name", receiver_name, ierr)
  call adios_get_scalar (fh, "/network", network, ierr)
  call adios_get_scalar (fh, "/component", component, ierr)
  call adios_get_scalar (fh, "/receiver_id", receiver_id, ierr)

  !split the job, get the location based on station info provided
  !print *, "split job"
  call split_job_mpi_on_sta_info(nrecords_total, receiver_name,&
    receiver_name_len, network, network_len, component, component_len, &
    receiver_id, receiver_id_len, nrecords, sta, nw, comp, rid, &
    points_array)
  !print *, "points_array:"
  !print *, points_array(:)
  print *, "nrecords, rank:", nrecords, rank
  if(nrecords.gt.MAXDATA_PER_PROC) then
    print *,"nrecords exceed MAXDATA_PER_PROC. Modify MAXDATA_PER_PROC."
    call MPI_ABORT(comm, errorcode, ierr)
  endif

  !>allocate variables
  call init_asdf_data(my_asdf, nrecords, resp_flag)

  my_asdf%receiver_name_array(1:nrecords) = sta(1:nrecords)
  my_asdf%network_array(1:nrecords) = nw(1:nrecords)
  my_asdf%component_array(1:nrecords) = comp(1:nrecords)
  my_asdf%receiver_id_array(1:nrecords) = rid(1:nrecords)

  !-----------------------------------------------
  !>read all the records 
  do i=1, nrecords
    loc_string=trim(my_asdf%receiver_name_array(i))//"."//&
            trim(my_asdf%network_array(i))//"."//&
            trim(my_asdf%component_array(i))//"."//&
            trim(my_asdf%receiver_id_array(i))
    if(points_array(i).gt.0)then
      !print *, trim(loc_string), dims(1)
      call adios_get_scalar(fh, trim(loc_string)//"/global_dim",dims(1), ierr)
      !print *, i,"dim:", dims(1)
      allocate (my_asdf%records(i)%record(dims(1)))
      my_asdf%npoints(i)=dims(1)
      start(1) = 0
      count(1) = dims(1)
      call adios_selection_boundingbox (sel_record, 1 , start , count )
      call adios_schedule_read (fh, sel_record, trim(loc_string)//"/array", 0, 1, &
            my_asdf%records(i)%record, ierr)
    else
      !if the record doesn't exist, then just creat a non-meaning array
      my_asdf%npoints(i)=1
      allocate (my_asdf%records(i)%record(1))
      my_asdf%records(i)%record(1)=0.0
    endif
    ! Read in instrument response
    if(my_asdf%STORE_RESPONSE) then
      loc_string="RESP."//trim(my_asdf%network_array(i))//"."//&
                 trim(my_asdf%receiver_name_array(i))//"."//&
                 trim(my_asdf%receiver_id_array(i))//"."//&
                 trim(my_asdf%component_array(i))
      call adios_get_scalar(fh, trim(loc_string)//"/global_dim", dims(1), ierr)
      my_asdf%responses(i)%response_length = dims(1)
      start(1) = 0
      count(1) = dims(1)
      call adios_selection_boundingbox (sel, 1 , start , count )
      call adios_schedule_read (fh, sel, trim(loc_string)//"/array", 0, 1, &
                               my_asdf%responses(i)%response_string, ierr)
    endif
  enddo

  !print *,"reading records finished!"

  !--------------------------------------
  !>read in earthquake information
  call adios_get_scalar (fh, "/event", my_asdf%event, ierr)  
  call adios_get_scalar (fh, "/nreceivers", my_asdf%nreceivers, ierr)
  !print *,"/event",my_asdf%event
  !print *,"nreceiver:",my_asdf%nreceivers

  !print *, "my_asdf%event:",my_asdf%event

  call adios_get_scalar (fh, "/max_period", my_asdf%max_period, ierr)
  call adios_get_scalar (fh, "/min_period", my_asdf%min_period, ierr)

  do i=1, nrecords
    if(points_array(i).gt.0)then
      !the offset array starts from 0
      !the points_array starts from 1
      !so subtract 1
      points_array_temp(i)=points_array(i)-1
    else
      !if the array doesn't exist(points_array==0), just fake to read the first
      !element. and then fix it later
      points_array_temp(i)=0
    endif
  enddo
  npoints_read=nrecords
  call adios_selection_points(sel, 1, npoints_read, points_array_temp)

  call adios_schedule_read (fh, sel, "gmt_year/array", 0, 1, my_asdf%gmt_year, ierr)
  call adios_schedule_read (fh, sel, "gmt_day/array", 0, 1, my_asdf%gmt_day, ierr)
  call adios_schedule_read (fh, sel, "gmt_hour/array", 0, 1, my_asdf%gmt_hour, ierr)
  call adios_schedule_read (fh, sel, "gmt_min/array", 0, 1, my_asdf%gmt_min, ierr)
  call adios_schedule_read (fh, sel, "gmt_sec/array", 0, 1, my_asdf%gmt_sec, ierr)
  call adios_schedule_read (fh, sel, "gmt_msec/array", 0, 1, my_asdf%gmt_msec, ierr)

  call adios_schedule_read (fh, sel, "event_lat/array", 0, 1, my_asdf%event_lat, ierr)
  call adios_schedule_read (fh, sel, "event_lo/array", 0, 1, my_asdf%event_lo, ierr)
  call adios_schedule_read (fh, sel, "event_dpt/array", 0, 1, my_asdf%event_dpt, ierr)

  call adios_schedule_read (fh, sel, "receiver_lat/array", 0, 1, my_asdf%receiver_lat, ierr)
  call adios_schedule_read (fh, sel, "receiver_lo/array", 0, 1, my_asdf%receiver_lo, ierr)
  call adios_schedule_read (fh, sel, "receiver_el/array", 0, 1, my_asdf%receiver_el, ierr)
  call adios_schedule_read (fh, sel, "receiver_dpt/array", 0, 1, my_asdf%receiver_dpt, ierr)
  call adios_schedule_read (fh, sel, "begin_value/array", 0, 1, my_asdf%begin_value, ierr)
  call adios_schedule_read (fh, sel, "end_value/array", 0, 1, my_asdf%end_value, ierr)
  call adios_schedule_read (fh, sel, "cmp_azimuth/array", 0, 1, my_asdf%cmp_azimuth, ierr)
  call adios_schedule_read (fh, sel, "cmp_incident_ang/array", 0, 1, my_asdf%cmp_incident_ang, ierr)
  call adios_schedule_read (fh, sel, "sample_rate/array", 0, 1, my_asdf%sample_rate, ierr)
  call adios_schedule_read (fh, sel, "scale_factor/array", 0, 1, my_asdf%scale_factor, ierr)
  call adios_schedule_read (fh, sel, "ev_to_sta_AZ/array", 0, 1, my_asdf%ev_to_sta_AZ, ierr)
  call adios_schedule_read (fh, sel, "sta_to_ev_AZ/array", 0, 1, my_asdf%sta_to_ev_AZ, ierr)
  call adios_schedule_read (fh, sel, "great_circle_arc/array", 0, 1, my_asdf%great_circle_arc, ierr)
  call adios_schedule_read (fh, sel, "dist/array", 0, 1, my_asdf%dist, ierr)
  call adios_schedule_read (fh, sel, "P_pick/array", 0, 1, my_asdf%P_pick, ierr)
  call adios_schedule_read (fh, sel, "S_pick/array", 0, 1, my_asdf%S_pick, ierr)

  call adios_perform_reads (fh, ierr)
  
  !print *, "my_asdf%npoints:"
  !print *, my_asdf%npoints(:)

  call adios_read_close(fh, ierr)
  call adios_read_finalize_method(ADIOS_READ_METHOD_BP, adios_err)

  call MPI_Barrier(comm, ierr)

end subroutine read_asdf_file_1

subroutine read_asdf_file_2(file_name, my_asdf, nrecords, &
    sta, nw, comp, rid, &
    rank, nproc, comm, ierr)
  
  !use asdf_data

  character(len=100),intent(in) :: file_name
  type(asdf_event), intent(inout) :: my_asdf
  integer :: nrecords
  character(len=*) :: sta(:), nw(:), comp(:), rid(:)
  integer,intent(in) :: rank, nproc, comm
  !logical,intent(in) :: resp_flag
  logical :: resp_flag
  integer :: ierr

end subroutine read_asdf_file_2

subroutine split_string(string, string_len, string_array, dim_array, delimiter)
!split the string into string array
!input: string, string_len, delimiter
!output: string_array, dim_array

  character(len=*) :: string
  character(len=*) :: delimiter
  character(len=*) :: string_array(:)
  integer :: dim_array
  integer :: string_len
  character(len=:), allocatable :: string_temp

  integer :: i1,i2,i3,i

  !print *,"split begins"

  allocate(character(len=len(string))::string_temp)
  
  string_array(:)=''
  string_temp=''
  dim_array=0

  i1=1
  i3=string_len

  do while (i1<=i3)
    i2=index(string(i1:i3),delimiter)
    dim_array=dim_array+1
    string_array(dim_array)(1:(i2-1))=string(i1:(i1+i2-2))
    i1=i1+i2
  enddo
  !print *, dim_array

end subroutine split_string

subroutine init_asdf_data(my_asdf,nrecords,resp_flag)
!init the asdf data structure
!receiver_name, network, component and receiver_id are not allocated

  !use asdf_data
  type(asdf_event) :: my_asdf
  integer :: nrecords
  integer :: len_temp
  logical,intent(in) :: resp_flag

  my_asdf%nrecords=nrecords

  !if (resp_flag) then
  !  my_asdf%STORE_RESPONSE = .TRUE.
  !else
  !  my_asdf%STORE_RESPONSE = .FALSE.
  !endif
  my_asdf%STORE_RESPONSE = resp_flag

  my_asdf%event = ""
  !print *,"Number of Records:", my_asdf%nrecords
  !>allocate array variables
  allocate (my_asdf%npoints(my_asdf%nrecords))
  allocate (my_asdf%gmt_year(my_asdf%nrecords))
  allocate (my_asdf%gmt_hour(my_asdf%nrecords))
  allocate (my_asdf%gmt_day(my_asdf%nrecords))
  allocate (my_asdf%gmt_min(my_asdf%nrecords))
  allocate (my_asdf%gmt_sec(my_asdf%nrecords))
  allocate (my_asdf%gmt_msec(my_asdf%nrecords))

  allocate (my_asdf%event_lat(my_asdf%nrecords))
  allocate (my_asdf%event_lo(my_asdf%nrecords))
  allocate (my_asdf%event_dpt(my_asdf%nrecords))

  allocate (my_asdf%receiver_lat(my_asdf%nrecords))
  allocate (my_asdf%receiver_lo(my_asdf%nrecords))
  allocate (my_asdf%receiver_el(my_asdf%nrecords))
  allocate (my_asdf%receiver_dpt(my_asdf%nrecords))
  allocate (my_asdf%begin_value(my_asdf%nrecords))
  allocate (my_asdf%end_value(my_asdf%nrecords))
  allocate (my_asdf%cmp_azimuth(my_asdf%nrecords))
  allocate (my_asdf%cmp_incident_ang(my_asdf%nrecords))
  allocate (my_asdf%sample_rate(my_asdf%nrecords))
  allocate (my_asdf%scale_factor(my_asdf%nrecords))
  allocate (my_asdf%ev_to_sta_AZ(my_asdf%nrecords))
  allocate (my_asdf%sta_to_ev_AZ(my_asdf%nrecords))
  allocate (my_asdf%great_circle_arc(my_asdf%nrecords))
  allocate (my_asdf%dist(my_asdf%nrecords))
  allocate (my_asdf%P_pick(my_asdf%nrecords))
  allocate (my_asdf%S_pick(my_asdf%nrecords))
  !>the kernel part: allocate the record
  allocate (my_asdf%records(my_asdf%nrecords))
  allocate (my_asdf%responses(my_asdf%nrecords))
  allocate (my_asdf%receiver_name_array(my_asdf%nrecords))
  allocate (my_asdf%network_array(my_asdf%nrecords))
  allocate (my_asdf%component_array(my_asdf%nrecords))
  allocate (my_asdf%receiver_id_array(my_asdf%nrecords))
  len_temp=6*nrecords
  my_asdf%min_period=0.0
  my_asdf%max_period=0.0

end subroutine init_asdf_data

subroutine deallocate_asdf(asdf_container, ierr)

  integer :: ierr
  type(asdf_event),intent(inout) :: asdf_container
  integer :: i

  deallocate (asdf_container%gmt_hour, STAT=ierr)
  if (ierr /= 0) stop "Failed to deallocate "
  deallocate (asdf_container%gmt_day, STAT=ierr)
  if (ierr /= 0) stop "Failed to deallocate "
  deallocate (asdf_container%gmt_min, STAT=ierr)
  if (ierr /= 0) stop "Failed to deallocate "
  deallocate (asdf_container%gmt_sec, STAT=ierr)
  if (ierr /= 0) stop "Failed to deallocate "
  deallocate (asdf_container%gmt_msec, STAT=ierr)
  if (ierr /= 0) stop "Failed to deallocate "
  deallocate (asdf_container%event_lat, STAT=ierr)
  if (ierr /= 0) stop "Failed to deallocate "
  deallocate (asdf_container%event_lo, STAT=ierr)
  if (ierr /= 0) stop "Failed to deallocate "
  deallocate (asdf_container%event_dpt, STAT=ierr)
  if (ierr /= 0) stop "Failed to deallocate "
  deallocate (asdf_container%receiver_lat, STAT=ierr)
  if (ierr /= 0) stop "Failed to deallocate "
  deallocate (asdf_container%receiver_lo, STAT=ierr)
  if (ierr /= 0) stop "Failed to deallocate "
  deallocate (asdf_container%receiver_el, STAT=ierr)
  if (ierr /= 0) stop "Failed to deallocate "
  deallocate (asdf_container%receiver_dpt, STAT=ierr)
  if (ierr /= 0) stop "Failed to deallocate "
  deallocate (asdf_container%begin_value, STAT=ierr)
  if (ierr /= 0) stop "Failed to deallocate "
  deallocate (asdf_container%end_value, STAT=ierr)
  if (ierr /= 0) stop "Failed to deallocate "
  deallocate (asdf_container%cmp_azimuth, STAT=ierr)
  if (ierr /= 0) stop "Failed to deallocate "
  deallocate (asdf_container%cmp_incident_ang, STAT=ierr)
  if (ierr /= 0) stop "Failed to deallocate "
  deallocate (asdf_container%sample_rate, STAT=ierr)
  if (ierr /= 0) stop "Failed to deallocate "
  deallocate (asdf_container%scale_factor, STAT=ierr)
  if (ierr /= 0) stop "Failed to deallocate "
  deallocate (asdf_container%ev_to_sta_AZ, STAT=ierr)
  if (ierr /= 0) stop "Failed to deallocate "
  deallocate (asdf_container%sta_to_ev_AZ, STAT=ierr)
  if (ierr /= 0) stop "Failed to deallocate "
  deallocate (asdf_container%great_circle_arc, STAT=ierr)
  if (ierr /= 0) stop "Failed to deallocate "
  deallocate (asdf_container%dist, STAT=ierr)
  if (ierr /= 0) stop "Failed to deallocate "
  deallocate (asdf_container%P_pick, STAT=ierr)
  if (ierr /= 0) stop "Failed to deallocate "
  deallocate (asdf_container%S_pick, STAT=ierr)
  if (ierr /= 0) stop "Failed to deallocate "
  do i = 1, asdf_container%nrecords
    deallocate(asdf_container%records(i)%record, STAT=ierr)
    if (ierr /= 0) stop "Failed to deallocate "
  enddo
  deallocate (asdf_container%receiver_name_array, STAT=ierr)
  if (ierr /= 0) stop "Failed to deallocate "
  deallocate (asdf_container%network_array, STAT=ierr)
  if (ierr /= 0) stop "Failed to deallocate "
  deallocate (asdf_container%component_array, STAT=ierr)
  if (ierr /= 0) stop "Failed to deallocate "
  deallocate (asdf_container%receiver_id_array, STAT=ierr)
  if (ierr /= 0) stop "Failed to deallocate "

end subroutine deallocate_asdf

subroutine split_job_mpi_simple(nrecords_total, nrecords_local, loc_begin, loc_end, &
              rank,nproc)

  !simplified version the mpi_job_split, strictly evenly splitted.
  !doesn't guarantee the 3 components on the same processor
  !if you want 3 components on the same processor, try using split_job_mpi_complex
  implicit none

  integer :: nrecords_total, nrecords_local, loc_begin, loc_end, rank, nproc

  integer :: nrecords_per_proc

  nrecords_per_proc=int(nrecords_total/nproc)

  loc_begin = nrecords_per_proc*rank+1
  loc_end = nrecords_per_proc*(rank+1)

  if(rank.eq.(nproc-1)) then !last node
    loc_end=nrecords_total
  endif

  nrecords_local=loc_end-loc_begin+1
  
  print *, "rank, loc_begin, loc_end:", rank, loc_begin, loc_end, &
              nrecords_per_proc, nproc, rank

  !stop
end subroutine split_job_mpi_simple

subroutine split_job_mpi_complex(nrecords_total,receiver_name, receiver_name_len, &
              nrecords_local, loc_begin, loc_end, rank, nproc)
!much like the split_job_mpi_simple, the only difference is to make 3 components
!at same receiver

  integer :: nrecords_total
  character(len=*) :: receiver_name
  integer :: receiver_name_len
  
  integer :: nrecords_local
  integer :: loc_begin, loc_end
  integer :: rank, nproc

  integer :: nrecords_per_proc
  character(len=20), allocatable :: receiver_name_array(:)
  integer :: dim_info

  integer :: loc_b(nproc), loc_e(nproc)
  integer :: i

  allocate(receiver_name_array(nrecords_total))

  call split_string(receiver_name, receiver_name_len, receiver_name_array, &
                        dim_info, '.')

  nrecords_per_proc=int(nrecords_total/nproc)

  do i=1,nproc
    loc_b(i) = nrecords_per_proc*(i-1)+1
    loc_e(i) = nrecords_per_proc*(i)
  enddo

  loc_e(nproc)=nrecords_total

  do i=1, nproc-1
    do while(trim(receiver_name_array(loc_e(i))).eq.&
                        trim(receiver_name_array(loc_e(i)+1)))
      loc_e(i)=loc_e(i)+1
    enddo
  enddo

  do i=2, nproc
    loc_b(i)=loc_e(i-1)+1
  enddo

  loc_begin=loc_b(rank+1)
  loc_end=loc_e(rank+1)
  !print *,"loc_begin, loc_end:", loc_begin, loc_end
  nrecords_local=loc_end-loc_begin+1

end subroutine split_job_mpi_complex

subroutine split_job_mpi_on_sta_info(nrecords_total, receiver_name,&
    receiver_name_len, network, network_len, component, component_len, &
    !receiver_id, receiver_id_len, nrecords, sta_re, nw_re, comp_re, rid_re, &
    receiver_id, receiver_id_len, nrecords, sta, nw, comp, rid, &
    points_array)
!split the job based on the station information provided.
!the main goal of this subroutine is to locate the position of specifi station 
!in the array

  integer :: nrecords_total, nrecords
  integer :: receiver_name_len, network_len, component_len, receiver_id_len
  character(len=*) :: receiver_name, network, component, receiver_id
  character(len=*) :: sta(:), nw(:), comp(:), rid(:)
  integer(kind=8) :: points_array(:)

  character(len=:), allocatable :: sta_total(:), nw_total(:), comp_total(:), rid_total(:)
  type(sta_info) :: sta_list(nrecords), sta_total_list(nrecords_total)
  integer :: index_list(128), index_ascii, start_loc, end_loc
  integer :: dim_array
  
  integer :: i,j

  allocate(character(len=20) :: sta_total(nrecords_total))
  allocate(character(len=20) :: nw_total(nrecords_total))
  allocate(character(len=20) :: comp_total(nrecords_total))
  allocate(character(len=20) :: rid_total(nrecords_total))

  call split_string(receiver_name,receiver_name_len, &
                            sta_total,dim_array,'.')
  call split_string(network,network_len, &
                            nw_total,dim_array,'.')
  call split_string(component,component_len, &
                            comp_total,dim_array,'.')
  call split_string(receiver_id,receiver_id_len, &
                            rid_total,dim_array,'.')

                            
!  print *,"here"
  do i=1, nrecords_total
    sta_total_list(i)%sta=sta_total(i)
    sta_total_list(i)%nw=nw_total(i)
    sta_total_list(i)%comp=comp_total(i)
    sta_total_list(i)%rid=rid_total(i)
    sta_total_list(i)%loc=i
  enddo

!  print *,"here"
  do i=1, nrecords
    sta_list(i)%sta=sta(i)
    sta_list(i)%nw=nw(i)
    sta_list(i)%comp=comp(i)
    sta_list(i)%rid=rid(i)
    sta_list(i)%loc=i
  enddo

  call insertion_sort(sta_total_list, nrecords_total)

  index_list(:)=0
  do i=1, nrecords_total
    index_ascii=iachar(sta_total_list(i)%sta(1:1))+1
    index_list(index_ascii)=index_list(index_ascii)+1
  enddo

  do i=2,128
    index_list(i)=index_list(i-1)+index_list(i)
  enddo

  points_array(:)=-1
  
  do i=1, nrecords
    index_ascii=iachar(sta_list(i)%sta(1:1))+1
    if(index_ascii.gt.1)then
      start_loc=index_list(index_ascii-1)+1
    else
      start_loc=1
    endif
    end_loc=index_list(index_ascii)
    do j=start_loc, end_loc
      if( trim(sta_list(i)%sta).eq.trim(sta_total_list(j)%sta) .and. &
         trim(sta_list(i)%nw).eq.trim(sta_total_list(j)%nw) ) then
        if( trim(sta_list(i)%comp(3:3)).eq.trim(sta_total_list(j)%comp(3:3)) )then
          points_array(i)=sta_total_list(j)%loc
          comp(i)(1:2)=sta_total_list(j)%comp(1:2)
          rid(i)=sta_total_list(j)%rid
        endif
      endif
    enddo
  enddo

end subroutine split_job_mpi_on_sta_info

subroutine insertion_sort(sta_list, nrecords)
!insertation sort algorithm. Sort the station list
!Sorting the station list makes the search faster

  type(sta_info) :: sta_list(:), sta_temp
  integer :: nrecords
  integer :: insert_loc
  integer :: compare_result

  integer :: i,j,k

!  print *,"insert sort"
  do i=2, nrecords
    sta_temp=sta_list(i)
    !get into sorted array
    do j=i-1,1,-1
      compare_result=compare_sta_info(sta_temp, sta_list(j))
      !print *, "i,j,result:", i, j, compare_result
      if(compare_result.eq.-1)then
        insert_loc=j
      else if(compare_result.eq.1)then
        insert_loc=j+1
        exit
      endif
  !      print *, insert_loc
    enddo
    !print *, "insert_loc:",insert_loc
    !move
    do j=i-1, insert_loc, -1
      sta_list(j+1)=sta_list(j)
    enddo
    sta_list(insert_loc)=sta_temp
  enddo
  
end subroutine insertion_sort

integer function compare_sta_info(sta1, sta2)
!compare the sta1 and sta2
!if sta1>sta2, return 1; if sta1<sta2, return 0;
!if sta1=sta2, return 0
!used in insertion_sort

  type(sta_info) :: sta1, sta2

  if( trim(sta1%sta).gt.trim(sta2%sta) ) then
    compare_sta_info = 1
  elseif( trim(sta1%sta).lt.trim(sta2%sta) ) then
    compare_sta_info = -1
  else
    if( trim(sta1%nw).gt.trim(sta2%nw) ) then
      compare_sta_info=1
    elseif( trim(sta1%nw).lt.trim(sta2%nw) ) then
      compare_sta_info=-1
    else
      if( trim(sta1%rid).gt.trim(sta2%rid) ) then
        compare_sta_info=1
      elseif( trim(sta1%rid).lt.trim(sta2%rid) )then
        compare_sta_info=-1
      else
        if( trim(sta1%comp).gt.trim(sta2%comp) )then
          compare_sta_info=1
        elseif( trim(sta1%comp).lt.trim(sta2%comp) ) then
          compare_sta_info=-1
        else
          compare_sta_info=0
        endif
      endif
    endif
  endif

  !print *, trim(sta1%sta), trim(sta2%sta), compare_sta_info

end function compare_sta_info

end module asdf_read_subs
