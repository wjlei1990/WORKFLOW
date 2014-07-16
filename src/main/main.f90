program main

  use asdf_data
  use asdf_read_subs
  use asdf_write_subs

  use flexwin_struct
  use ma_struct
   
  use fw_interface
  use ma_interface
  use process_asdf_mod

  use main_subs

  use var_main

  use mpi
  implicit none
  
  type(asdf_event) :: obsd_proc, synt_proc
  type(asdf_event) :: synt_all, obsd_all, synt_phydisp_all
  type(asdf_event) :: adj_all
  type(win_info), allocatable :: win_all(:)

  integer :: nproc, comm, rank
  integer :: ierr, adios_err
  integer(kind=8) :: adios_group

  character(len=32) :: station(MAXDATA_PER_PROC), network(MAXDATA_PER_PROC)
  character(len=32) :: component(MAXDATA_PER_PROC), receiver_id(MAXDATA_PER_PROC)

  integer :: nrecords

  call MPI_INIT(ierr)
  call MPI_COMM_DUP(mpi_comm_world, comm, ierr)
  call MPI_COMM_RANK(comm, rank, ierr)
  call MPI_COMM_SIZE(comm, nproc, ierr)

  call adios_init_noxml(comm, adios_err)
  call adios_allocate_buffer(600, adios_err)
  call adios_declare_group(adios_group, "EVENTS", "iter", 1, adios_err)
  call adios_select_method(adios_group, "MPI", "", "", adios_err)

  !==========================
  !read parfile of main
  call read_main_parfile(rank, comm, ierr)
  
  !==========================
  !read obsd and synt file
  call read_asdf_file(OBSD_FILE, obsd_all, nrecords, &
    station, network, component, receiver_id, 0, &
    rank, nproc, comm, ierr)
  call read_asdf_file(SYNT_FILE, synt_all, nrecords, &
    station, network, component, receiver_id, 1, &
    rank, nproc, comm, ierr)
  !if(USE_PHYDISP)then
  !  call read_asdf_file(SYNT_FILE, synt_phydisp_all, nrecords, &
  !    station, network, component, receiver_id, 1, &
  !    rank, nproc, comm, ierr)
  !endif

  !=============================
  !preprocessing stage
  call process_asdf(obsd_all, synt_all, obsd_proc, synt_proc, rank, comm)

  stop

  call write_asdf_file ("obs_proc.bp", obsd_proc, adios_group, rank, nproc, comm, ierr)
  call write_asdf_file ("syn_proc.bp", synt_proc, adios_group, rank, nproc, comm, ierr)

  !shoule be removed in the future
  obsd_proc%min_period=MIN_PERIOD
  obsd_proc%max_period=MAX_PERIOD
  synt_proc%min_period=MIN_PERIOD
  synt_proc%max_period=MAX_PERIOD

  !=============================
  !flexwin
  call flexwin_interface(obsd_proc, synt_proc, win_all, &
    rank, nproc, comm, ierr)

  !=============================
  !measure_adj
  call measure_adj_interface(obsd_proc, synt_proc, synt_phydisp_all, &
    win_all, adj_all, rank, nproc, adios_group, comm, ierr)

  call MPI_Barrier(comm, ierr)
  call adios_finalize(rank, ierr)
  call MPI_Finalize(ierr)

end program main
