module asdf_write_subs

!> The module asdf_subs contains Parallel ASDF I/O API:
!! 1)define_asdf_data
!! 3)write_asdf_file
  use asdf_data
  implicit none

  !include "asdf_constants.h"

contains

!! \param nreceivers The number of receivers
!! \param adios_group adios group
!! \param my_group_size Stores the adios group size

subroutine define_asdf_data (adios_group, my_group_size, asdf_container, rank, nproc, comm, ierr)

  use adios_write_mod
  use adios_helpers_mod
  !use asdf_data
  implicit none

  integer, intent(in) :: rank, nproc, comm
  integer(kind=8), intent(in) :: adios_group
  type(asdf_event), intent(in) :: asdf_container
  integer, parameter :: STRING_COMMON_LENGTH = 20
  integer :: ierr
  integer :: i, nerr, string_total_length
  integer :: adios_err, stat
  integer(kind=8) :: my_group_size
  integer(kind=8) :: varid

  integer :: nrecords

  !character                    :: data_type, dummy_blank
  character(len=2)             :: data_type
  character(len=32)            :: header, record
  character(len=6)             :: npts_string
  character(len=10)            :: i_string
  character(len=200)           :: command, dummy, record_path

  byte :: byte_array
  integer :: dum_int, int_array(10)
  real    :: dum_real, real_array(10)
  real(kind=8) :: dum_double, double_array(10)
  logical :: dum_logical, logical_array(10)
  character(len=10) :: dum_string

  integer :: nrecords_total, offset
  !gather info. Here, we only need nrecords_total
  nrecords=asdf_container%nrecords
  call gather_offset_info(nrecords,nrecords_total,offset, rank, nproc, comm, ierr)

  call define_adios_local_string_1d_array (adios_group, my_group_size,  13, "", "event", dummy)

  !nrecords info
  call define_adios_scalar (adios_group, my_group_size, "", "nreceivers", dum_int)
  call define_adios_scalar (adios_group, my_group_size, "", "nrecords", dum_int)
  !frequency(period) info
  call define_adios_scalar (adios_group, my_group_size, "", "min_period", dum_double)
  call define_adios_scalar (adios_group, my_group_size, "", "max_period", dum_double)

  !string info
  call define_adios_scalar (adios_group, my_group_size, "", "receiver_name_len", &
                        dum_int)
  call define_adios_scalar (adios_group, my_group_size, "", "network_len", &
                        dum_int)
  call define_adios_scalar (adios_group, my_group_size, "", "receiver_id_len", &
                        dum_int)
  call define_adios_scalar (adios_group, my_group_size, "", "component_len", &
                        dum_int)

  !RESPONSE FLAG: store as integer cause logical is not supported 
  call define_adios_scalar (adios_group, my_group_size, "", "STORE_RESPONSE", &
                        dum_int)

  !print *, "TAG"
  !HEADER info
  open(5, file="./src/asdf_util/ASDF_HEADERS", iostat=stat, status='old')
  if(stat.ne.0)then
    print *,"Can not find ASDF_HEADERS"
    print *,"The default path: ./src/asdf_util/ASDF_HEADERS"
    print *,"Quit!"
    stop
  endif

  do
    read (5, *, iostat=stat) data_type, header
    if (stat /= 0) exit
    select case (data_type(1:1))
      case ("i")
        call define_adios_global_integer_1d_array (adios_group, my_group_size,&
                    nrecords, "", trim(header), int_array)
      case("r")
        call define_adios_global_real_1d_array (adios_group, my_group_size, &
                    nrecords, "", trim(header), real_array)
      case ("d")
        call define_adios_global_double_1d_array (adios_group, my_group_size, &
                    nrecords, "", trim(header), double_array)
      case ("s")
        !Needs to pay attention in the future...here
        !potential bugs
        string_total_length = STRING_COMMON_LENGTH * nrecords_total
        call define_adios_local_string_1d_array (adios_group, my_group_size,&
                    string_total_length, "", trim(header), dum_string)
    end select
  enddo
  close(5)

  !DISPLACEMENT
  do i = 1, nrecords
    write(i_string, '(I10)' ) i+offset
    record=trim(asdf_container%receiver_name_array(i))//"."//&
           trim(asdf_container%network_array(i))//"."//&
           trim(asdf_container%component_array(i))//"."//&
           trim(asdf_container%receiver_id_array(i))
   call define_adios_global_double_1d_array (adios_group, my_group_size,&
          asdf_container%npoints(i), "", trim(record),&
          double_array)
  enddo

  !INSTRUMENT RESPONSE
  if (asdf_container%STORE_RESPONSE) then
    do i = 1, nrecords
      write(i_string, '(I10)' ) i+offset
                record="RESP."//trim(asdf_container%network_array(i))//"."//&
                                               trim(asdf_container%receiver_name_array(i))//"."//&
                                               trim(asdf_container%receiver_id_array(i))//"."//&
                                               trim(asdf_container%component_array(i))
      call define_adios_global_byte_1d_array (adios_group, my_group_size,&
          asdf_container%responses(i)%response_length, "", trim(record), byte_array)
    enddo
  endif

  !define attribute
  call adios_define_attribute ( adios_group , "nreceivers", "desc", &
        adios_string, "Number of receivers ", "" , adios_err )
  call adios_define_attribute ( adios_group , "nrecords", "desc", &
        adios_string, "Number of records ", "" , adios_err ) 
  call adios_define_attribute ( adios_group , "min_period", "desc", &
        adios_string, "Low pass filter in Hz (0 if none applied)  ", "" , adios_err )
  call adios_define_attribute ( adios_group , "max_period", "desc", &
        adios_string, "High pass filter in Hz (0 if none applied)  ", "" , adios_err )
  call adios_define_attribute ( adios_group , "event_lat", "desc", adios_string, &
        "Event CMT latitude (degrees, north positive) ", "", adios_err )
  call adios_define_attribute ( adios_group , "event_lo", "desc", adios_string, &
        "Event CMT longitude (degrees, east positive) ", "", adios_err )
  call adios_define_attribute ( adios_group , "event_dpt", "desc", adios_string, &
        "Event CMT depth (km) ", "" , adios_err )
  call adios_define_attribute ( adios_group , "event_dpt", "desc", adios_string, &
        "Event CMT depth (km) ", "" , adios_err )
  call adios_define_attribute ( adios_group , "component", "desc", adios_string, &
        "Record component ", "" , adios_err)
  call adios_define_attribute ( adios_group, "gmt_year", "desc", adios_string, &
        "GMT year corresponding to reference (zero) time in file. ", "" , adios_err)
  call adios_define_attribute ( adios_group, "gmt_day", "desc", adios_string, &
        "GMT julian day corresponding to reference (zero) time in file. ", "" , adios_err)
  call adios_define_attribute ( adios_group, "gmt_hour", "desc", adios_string, &
        "GMT hour corresponding to reference (zero) time in file. ", "" , adios_err)
  call adios_define_attribute ( adios_group, "gmt_min", "desc", adios_string, &
        "GMT minute corresponding to reference (zero) time in file. ", "" , adios_err)
  call adios_define_attribute ( adios_group, "gmt_sec", "desc", adios_string, &
        "GMT second corresponding to reference (zero) time in file. ", "" , adios_err)
  call adios_define_attribute ( adios_group, "gmt_msec", "desc", adios_string, &
        "GMT millisecond corresponding to reference (zero) time in file. ", "" , adios_err)
  call adios_define_attribute ( adios_group , "receiver_lat", "desc", adios_string, &
        "Receiver latitude (degrees, north positive)  ", "" , adios_err )
  call adios_define_attribute ( adios_group , "receiver_lo", "desc", adios_string, &
        "Receiver longitude (degrees, east positive) ", "" , adios_err )
  call adios_define_attribute ( adios_group , "receiver_dpt", "desc", adios_string, &
        "Receiver depth below surface (meters) ", "" , adios_err )
  call adios_define_attribute ( adios_group , "receiver_el", "desc", adios_string, &
        "Receiver elevation (meters) ", "" , adios_err )
  call adios_define_attribute ( adios_group , "begin_value", "desc", adios_string, &
        "Beginning value of time array ", "" , adios_err )
  call adios_define_attribute ( adios_group , "end_value", "desc", adios_string, &
        "End value of time array ", "" , adios_err )
  call adios_define_attribute ( adios_group , "cmp_azimuth", "desc", adios_string, &
        "Component azimuth (degrees clockwise from north) ", "", adios_err )
  call adios_define_attribute ( adios_group , "cmp_incident_ang", "desc", adios_string,&
        "Component incident angle (degrees from vertical) ", "", adios_err )
  call adios_define_attribute ( adios_group , "sample_rate", "desc", adios_string, &
        "Sampling rate (s) ", "" , adios_err )
  call adios_define_attribute ( adios_group , "scale_factor", "desc", adios_string, &
        "Scale factor to convert the unit of synthetics from meters to nanometer ", &
        "" , adios_err )
  call adios_define_attribute ( adios_group , "ev_to_sta_AZ", "desc", adios_string, &
        "Event to station azimuth (degrees) ", "" , adios_err )
  call adios_define_attribute ( adios_group , "sta_to_ev_AZ", "desc", adios_string, &
        "Station to event azimuth (backazimuth, degrees) ", "", adios_err )
  call adios_define_attribute ( adios_group , "great_circle_dist", "desc", adios_string, &
        "Great circle distance between event and station (degrees) ", "", adios_err )
  call adios_define_attribute ( adios_group , "receiver_name", "desc", adios_string, &
        "Receiver name ", "" , adios_err )
  call adios_define_attribute( adios_group , "network", "desc", adios_string, &
        "Receiver network name ", "" , adios_err )
  call adios_define_attribute( adios_group , "receiver_id", "desc", adios_string, &
        "Receiver number ", "" , adios_err )
  call adios_define_attribute ( adios_group , "component", "desc", adios_string,&
        "Receiver component name ", "" , adios_err )

end subroutine define_asdf_data

!> Writes sac data to an asdf data file
!! \param file_name The file will be saved as file_name.
!! \param comm Size of the group associated with the MPI communicator

subroutine write_asdf_file(asdf_fn, asdf_container, adios_group, rank, nproc, comm, ierr)

  !use asdf_data
  use adios_write_mod

  character(len=*) :: asdf_fn 
  type(asdf_event) :: asdf_container
  integer :: rank, nproc, comm, ierr

  integer        :: adios_err
  integer(kind=8)         :: adios_groupsize, adios_totalsize, varid
  integer(kind=8)         :: adios_handle, adios_group

  !calculate size
  adios_groupsize = 0
  print *,"Write out file: ", trim(asdf_fn)
  print *, "Define adios data structure..."
  call define_asdf_data (adios_group, adios_groupsize, asdf_container,&
                         rank, nproc, comm, ierr)
  !print *, "define finished!"
  call adios_open (adios_handle, "EVENTS", asdf_fn, "w", comm, adios_err)
  call adios_group_size (adios_handle, adios_groupsize, adios_totalsize, adios_err)

  !call the write sub
  call write_asdf_file_sub (asdf_container, adios_handle, adios_group,&
                            adios_groupsize, rank, nproc, comm, ierr)

  !adios close
  call adios_close(adios_handle, adios_err)
  !print *, "adios_err", adios_err
  if(adios_err.eq.0) then
    print *, "Finish writing file:", trim(asdf_fn)
  endif
  !call adios_finalize (rank, adios_err)

end subroutine write_asdf_file

subroutine write_asdf_file_sub (asdf_container, adios_handle, my_adios_group, adios_groupsize, rank, nproc, comm, ierr)

  use adios_write_mod
  !use asdf_data
  use adios_helpers_writers_mod

  implicit none
  integer                       :: adios_err, i
  integer(kind=8),intent(in)    :: my_adios_group, adios_groupsize
  integer(kind=8),intent(in)    :: adios_handle
  integer,intent(in)            :: rank, nproc, comm, ierr
  integer :: nrecords_total, offset
  integer :: receiver_name_len, network_len, component_len, receiver_id_len
  integer :: rn_len_total, nw_len_total, rid_len_total, comp_len_total
  integer :: rn_offset, nw_offset, rid_offset, comp_offset
  character(len=32)              :: loc_string
  integer :: errorcode

  character(len=:), allocatable :: receiver_name, network, component, receiver_id
  !character(len=:), allocatable :: response
  character(len=:), allocatable :: receiver_name_total, network_total, &
                                  component_total, receiver_id_total

  type(asdf_event), intent(inout) :: asdf_container
  integer :: STORE_RESPONSE_INT

  !gather array offset info
  call gather_offset_info(asdf_container%nrecords,nrecords_total,offset,&
                          rank, nproc, comm, ierr)

  if(asdf_container%nrecords.gt.MAXDATA_PER_PROC)then
    print *,"nrecords on processer larger than MAXDATA_PER_PROC"
    call MPI_ABORT(comm, errorcode, ierr)
  endif
  !ensemble the string for receiver_name, network, componen and receiver_id
  allocate(character(len=6*MAXDATA_PER_PROC) :: receiver_name)
  allocate(character(len=6*MAXDATA_PER_PROC) :: network)
  allocate(character(len=6*MAXDATA_PER_PROC) :: component)
  allocate(character(len=6*MAXDATA_PER_PROC) :: receiver_id)
  receiver_name=''
  network=''
  component=''
  receiver_id=''

  do i=1, asdf_container%nrecords
    receiver_name=trim(receiver_name)//trim(asdf_container%receiver_name_array(i))//'.'
                network=trim(network)//trim(asdf_container%network_array(i))//'.'
                component=trim(component)//trim(asdf_container%component_array(i))//'.'
                receiver_id=trim(receiver_id)//trim(asdf_container%receiver_id_array(i))//'.'
  enddo
  receiver_name_len = len_trim(receiver_name)
  network_len = len_trim(network)
  component_len = len_trim(component)
  receiver_id_len = len_trim(receiver_id)

  call gather_string_offset_info(receiver_name_len, rn_len_total, rn_offset, &
                                 receiver_name, receiver_name_total,&
                                 rank, nproc, comm, ierr)
  call gather_string_offset_info(network_len, nw_len_total, nw_offset, &
                                 network, network_total,&
                                 rank, nproc, comm, ierr)
  call gather_string_offset_info(receiver_id_len, rid_len_total, rid_offset, &
                                 receiver_id, receiver_id_total,&
                                 rank, nproc, comm, ierr)
  call gather_string_offset_info(component_len, comp_len_total, comp_offset, &
                                 component, component_total,&
                                 rank, nproc, comm, ierr)

  !===========================
  !write out the string info
  print *,"write string"
  if(rank.eq.0)then
    call adios_write(adios_handle, "receiver_name", trim(receiver_name), adios_err)
    call adios_write(adios_handle, "network", trim(network), adios_err)
    call adios_write(adios_handle, "component", trim(component), adios_err)
    call adios_write(adios_handle, "receiver_id", trim(receiver_id), adios_err) 
  endif

  !===========================
  print *,"Write seismic record"
  do i = 1, asdf_container%nrecords
    !write( loc_string, '(I10)' ) i+offset
    loc_string=trim(asdf_container%receiver_name_array(i))//"."//&
               trim(asdf_container%network_array(i))//"."//&
               trim(asdf_container%component_array(i))//"."//&
               trim(asdf_container%receiver_id_array(i))
    call write_adios_global_double_1d_array(adios_handle, rank, nproc, &
                       asdf_container%npoints(i), asdf_container%npoints(i), 0, &
                       loc_string, asdf_container%records(i)%record)
  enddo

  !===========================
  if (asdf_container%STORE_RESPONSE) then
    print *,"Write instrument response"
    do i = 1, asdf_container%nrecords
      write( loc_string, '(I10)' ) i+offset
      loc_string="RESP."//trim(asdf_container%network_array(i))//"."//&
               trim(asdf_container%receiver_name_array(i))//"."//&
               trim(asdf_container%receiver_id_array(i))//"."//&
               trim(asdf_container%component_array(i))
      call write_adios_global_byte_1d_array(adios_handle, rank, nproc, &
                        asdf_container%responses(i)%response_length, &
                        asdf_container%responses(i)%response_length, 0, loc_string, &
                        trim(asdf_container%responses(i)%response_string(1:asdf_container%responses(i)%response_length)))
    enddo
  endif

  !===========================
  !scalar
  print *,"write scalar"
  !if(rank.eq.0)then
    call adios_write(adios_handle, "nrecords", nrecords_total, adios_err)
    call adios_write(adios_handle, "receiver_name_len", rn_len_total, adios_err)
    call adios_write(adios_handle, "network_len", nw_len_total, adios_err)
    call adios_write(adios_handle, "component_len", comp_len_total, adios_err)
    call adios_write(adios_handle, "receiver_id_len", rid_len_total, adios_err)
    call adios_write(adios_handle, "nreceivers", asdf_container%nreceivers, adios_err)
    call adios_write(adios_handle, "min_period", asdf_container%min_period, adios_err) 
    call adios_write(adios_handle, "max_period", asdf_container%max_period, adios_err) 
    call adios_write(adios_handle, "event", asdf_container%event, adios_err)
    !store_response are stored as integer  in asdf file
    if(asdf_container%STORE_RESPONSE)then
      STORE_RESPONSE_INT=1
    else
      STORE_RESPONSE_INT=0
    endif
    call adios_write(adios_handle, "STORE_RESPONSE", STORE_RESPONSE_INT, adios_err)
    !print *, "tag:",trim(asdf_container%event)
  !endif

  !===========================
  !write out the array using the offset info
  print *,"write array"
  call write_adios_global_integer_1d_array(adios_handle, rank, nproc, asdf_container%nrecords,&
        nrecords_total, offset, "npoints", asdf_container%npoints)

  call write_adios_global_integer_1d_array(adios_handle, rank, nproc, asdf_container%nrecords,&
        nrecords_total, offset, "gmt_year", asdf_container%gmt_year)
  call write_adios_global_integer_1d_array(adios_handle, rank, nproc, asdf_container%nrecords,&
        nrecords_total, offset, "gmt_day", asdf_container%gmt_day)
  call write_adios_global_integer_1d_array(adios_handle, rank, nproc, asdf_container%nrecords,&
        nrecords_total, offset, "gmt_hour", asdf_container%gmt_hour)
  call write_adios_global_integer_1d_array(adios_handle, rank, nproc, asdf_container%nrecords,&
        nrecords_total, offset, "gmt_min", asdf_container%gmt_min)
  call write_adios_global_integer_1d_array(adios_handle, rank, nproc, asdf_container%nrecords,&
        nrecords_total, offset, "gmt_sec", asdf_container%gmt_sec)
  call write_adios_global_integer_1d_array(adios_handle, rank, nproc, asdf_container%nrecords,&
        nrecords_total, offset, "gmt_msec", asdf_container%gmt_msec)

  call write_adios_global_double_1d_array(adios_handle, rank, nproc, asdf_container%nrecords, &
        nrecords_total, offset, "event_lat", asdf_container%event_lat)
  call write_adios_global_double_1d_array(adios_handle, rank, nproc, asdf_container%nrecords, &
        nrecords_total, offset, "event_lo", asdf_container%event_lo)
  call write_adios_global_double_1d_array(adios_handle, rank, nproc, asdf_container%nrecords, &
        nrecords_total, offset, "event_dpt", asdf_container%event_dpt)

  call write_adios_global_double_1d_array(adios_handle, rank, nproc, asdf_container%nrecords, &
        nrecords_total, offset, "receiver_lat", asdf_container%receiver_lat)
  call write_adios_global_double_1d_array(adios_handle, rank, nproc, asdf_container%nrecords,& 
                                nrecords_total, offset, "receiver_lo", asdf_container%receiver_lo)
  call write_adios_global_double_1d_array(adios_handle, rank, nproc, asdf_container%nrecords,&
                                nrecords_total, offset, "receiver_el", asdf_container%receiver_el)
  call write_adios_global_double_1d_array(adios_handle, rank, nproc, asdf_container%nrecords,&
                                nrecords_total, offset, "receiver_dpt", asdf_container%receiver_dpt)

  call write_adios_global_double_1d_array(adios_handle, rank, nproc, asdf_container%nrecords,& 
                                nrecords_total, offset, "begin_value", asdf_container%begin_value)
  call write_adios_global_double_1d_array(adios_handle, rank, nproc, asdf_container%nrecords,& 
                                nrecords_total, offset, "end_value", asdf_container%end_value)

  call write_adios_global_double_1d_array(adios_handle, rank, nproc, asdf_container%nrecords,& 
                                nrecords_total, offset, "cmp_azimuth", asdf_container%cmp_azimuth)
  call write_adios_global_double_1d_array(adios_handle, rank, nproc, asdf_container%nrecords,& 
                                nrecords_total, offset, "cmp_incident_ang", asdf_container%cmp_incident_ang)
  call write_adios_global_double_1d_array(adios_handle, rank, nproc, asdf_container%nrecords,& 
                                nrecords_total, offset, "sample_rate", asdf_container%sample_rate)
  call write_adios_global_double_1d_array(adios_handle, rank, nproc, asdf_container%nrecords,&
                                nrecords_total, offset, "scale_factor", asdf_container%scale_factor)

  call write_adios_global_double_1d_array(adios_handle, rank, nproc, asdf_container%nrecords,&
                                nrecords_total, offset, "ev_to_sta_AZ", asdf_container%ev_to_sta_AZ)
  call write_adios_global_double_1d_array(adios_handle, rank, nproc, asdf_container%nrecords,& 
                                nrecords_total, offset, "sta_to_ev_AZ", asdf_container%sta_to_ev_AZ)
  call write_adios_global_double_1d_array(adios_handle, rank, nproc, asdf_container%nrecords,& 
                                nrecords_total, offset, "great_circle_arc", asdf_container%great_circle_arc)
  call write_adios_global_double_1d_array(adios_handle, rank, nproc, asdf_container%nrecords,& 
                                nrecords_total, offset, "dist", asdf_container%dist)

  call write_adios_global_double_1d_array(adios_handle, rank, nproc, asdf_container%nrecords,& 
                                nrecords_total, offset, "P_pick", asdf_container%P_pick)
  call write_adios_global_double_1d_array(adios_handle, rank, nproc, asdf_container%nrecords,& 
                                nrecords_total, offset, "S_pick", asdf_container%S_pick)

  !end of write_asdf_file_sub

end subroutine write_asdf_file_sub

subroutine gather_offset_info(local_dim, global_dim, offset,&
                                                rank, nproc, comm, ierr)

        use mpi
        implicit none

        integer :: local_dim, global_dim, offset
        integer :: rank, nproc, comm, ierr

        integer, allocatable :: local_dim_all_proc(:)
        integer, allocatable :: offset_all_proc(:)
        integer :: i

        !if(rank.eq.0)then
                allocate(local_dim_all_proc(nproc))
                allocate(offset_all_proc(nproc))
        !endif
        
        call MPI_Barrier(comm, ierr)

        call MPI_Gather(local_dim, 1, MPI_INTEGER, local_dim_all_proc, 1, &
                                        MPI_INTEGER, 0, comm, ierr)

        if(rank.eq.0)then
                offset_all_proc(1)=0
                do i=2, nproc
                        offset_all_proc(i)=sum(local_dim_all_proc(1:(i-1)))
                enddo
                global_dim=sum(local_dim_all_proc(1:nproc))
                !print *, "offset_all_proc:", offset_all_proc(:)
        endif

        call MPI_Scatter(offset_all_proc, 1, MPI_INTEGER, offset, &
                                        1, MPI_INTEGER, 0, comm, ierr)
        call MPI_Bcast(global_dim, 1, MPI_INTEGER, 0, comm, ierr)

        !print *,"rank, local dim, global_dim,offset:", rank, local_dim, &
        !                                                global_dim, offset

end subroutine gather_offset_info


subroutine gather_string_offset_info(local_dim, global_dim, offset,&
                                         string_piece, string_total,&
                                                rank, nproc, comm, ierr)

  use mpi
  implicit none

  integer :: local_dim, global_dim, offset
  character(len=*) :: string_piece
  character(len=:), allocatable :: string_total
  character(len=10000) :: buffer_string
 !character(len=:), allocatable :: buffer_string
  integer :: rank, nproc, comm, ierr, errorcode

  integer, allocatable :: local_dim_all_proc(:)
  integer, allocatable :: offset_all_proc(:)
  integer :: i, tag, mpi_status(MPI_STATUS_SIZE)

 !if(rank.eq.0)then
  allocate(local_dim_all_proc(nproc))
  allocate(offset_all_proc(nproc))
  !endif
        
  call MPI_Barrier(comm, ierr)

  call MPI_Gather(local_dim, 1, MPI_INTEGER, local_dim_all_proc, 1, &
                                        MPI_INTEGER, 0, comm, ierr)

  !allocate(character(len=10000) :: buffer_string )

  if(rank.eq.0)then
    offset_all_proc(1)=0
    do i=2, nproc
       offset_all_proc(i)=sum(local_dim_all_proc(1:(i-1)))
    enddo
    global_dim=sum(local_dim_all_proc(1:nproc))
    !print *, "offset_all_proc:", offset_all_proc(:)
    !=================
    !we make a constatn allocate here beacause the gfortran 4.7(or before)
    !doesn't support allocatable string with variable lenght
    if(global_dim.gt.(6*MAXDATA_TOTAL))then
      print *,"Gathered string lens greater than MAXDATA_TOTAL. Increase the MAXDATA_TOTAL"
      call MPI_ABORT(comm, errorcode, ierr)
    endif
    allocate(character(len=6*MAXDATA_TOTAL) :: string_total)
    !==================
    !allocate(character(len=global_dim) :: buffer_string)
    string_total=""
    buffer_string=""
    string_total=trim(string_total)//trim(string_piece(1:local_dim))
  endif
        
  !print *,"TAG1"
  !if(rank.eq.0) then
  !  print *,"global_dim",global_dim
  !endif

  if(rank.eq.0)then
    do i=1,nproc-1
    !print *, "buffer_before:",trim(buffer_string)
    !print *, "local_dim_all_proc:",local_dim_all_proc(i+1)
    call MPI_Recv(buffer_string, local_dim_all_proc(i+1), MPI_CHARACTER,&
                         i, 1, comm, mpi_status, ierr)
    !print *,"buffer_string:", trim(buffer_string)
     string_total=trim(string_total)//buffer_string(1:local_dim_all_proc(i+1))
    enddo
  else
    !print *, "local_dim:", local_dim
    !print *,"string_piece:", trim(string_piece)
    call MPI_Send(string_piece, local_dim, MPI_CHARACTER,&
                                               0, 1, comm, ierr)
  endif

  call MPI_Scatter(offset_all_proc, 1, MPI_INTEGER, offset, &
                                        1, MPI_INTEGER, 0, comm, ierr)
  call MPI_Bcast(global_dim, 1, MPI_INTEGER, 0, comm, ierr)

  !print *,"rank, local dim, global_dim,offset:", rank, local_dim, &
!                                                        global_dim, offset

end subroutine gather_string_offset_info


end module asdf_write_subs
