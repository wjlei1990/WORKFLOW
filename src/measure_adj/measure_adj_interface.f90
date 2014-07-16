module ma_interface

contains
!==============================================================================
!Program: Global_Tomography_Data_Processing
!Developer: Princeton Global Tomography Group(PGTG)
!Group Member: Wenjie Lei(lei@princeton.edu), Ebru Bozdag(bozdag@princeton.edu),
!James A. Smith(jas11@princeton.edu)
!===================
!Functions:
!1) Read ADIOS
!2) Processing: rtrend, rmean, taper, inst_remove(filter)
!3) data_quality: generate the useful data list
!4) flexwin: window selection
!5) Measure_adj: make measurements and adjoint sources
!5) write out the new ADIOS file(filtered, selected and window selected)
!===================
!Bug Report: lei@princeton.edu
!===============================================================================

subroutine measure_adj_interface(obsd_all, synt_all, synt_phydisp_all, &
  win_all, adj_all, rank, nproc, adios_group, comm, ierr)

  use asdf_data
  use flexwin_struct
  use ma_struct

  use win_io_subs
  use asdf_read_subs
  use asdf_write_subs

  use measure_adj_subs
  use rotate_subs
  use mpi_weighting_subs

  use var_main
  use measure_adj_interface_subs

  use mpi
  implicit none

  type(asdf_event)        :: synt_all, obsd_all, synt_phydisp_all
  type(asdf_event)        :: adj_all, adj_all_rotate
  type(win_info),allocatable      :: win_all(:)
  type(win_chi_info), allocatable :: win_chi_all(:)
  type(ma_par_struct_all) 				:: measure_adj_par_all
  type(ma_weighting_par_struct) 	:: ma_weighting_par

  character(len=200) :: ADJ_FILE

  integer :: nrecords
  character(len=150) :: ma_outdir

  double precision, allocatable :: adj_source(:)
  
  !mpi_var
  integer                 :: nproc,comm,rank
  integer                 :: ierr, adios_err
  !adios_var
  integer(kind=8)         :: adios_group

  integer                 :: i

  double precision :: t1, t2

  character(len=20) :: p1_string, p2_string

  t1=MPI_WTIME()

  !--------------------------'
	if(rank.eq.0) then
    print *, "================="
    print *, "BEGIN MEASURE_ADJ"
	endif
  !read phydisp synthetic file if needed
  !nrecords=obsd_all%nrecords
  if(.not.USE_PHYDISP)then
    !if no, just init synt_phydisp_all, and use it as dummy
    call init_asdf_data(synt_phydisp_all,obsd_all%nrecords, .false.) 
    do i=1,obsd_all%nrecords
      allocate(synt_phydisp_all%records(i)%record(1))
    enddo
  endif
	if(rank.eq.0) then
  	print *, "/event:", trim(obsd_all%event)
	endif

  !-------------------------------------.
  !read  measure_adj parfile        
  !-------------------------------------'
  if(rank.eq.0) then
    print *,"----------------------------------"
    print *,"Read in Measure_Adj Parfile..."
    print *,"----------------------------------"
  endif
  call read_ma_parfile_mpi(measure_adj_par_all, obsd_all%min_period,&
        obsd_all%max_period, obsd_all%event_dpt, obsd_all%nrecords,&
        USE_PHYDISP, rank, comm, ierr)

	call MPI_Barrier(comm,ierr)

  !allocate(win_all(obsd_all%nrecords))
  allocate(win_chi_all(obsd_all%nrecords))

	call MPI_Barrier(comm, ierr)

  !--------------------------.
  !measure_adj               !
  !--------------------------'
	if(rank.eq.0)then
   	print *,"---------------------"
   	print *,"RUNNING Meassure_adj "
   	print *,"---------------------"
   	print *,"Weighting Begin!"
	endif

  !setup weighting based on the whole dataset
  call setup_measure_adj_weighting_asdf_mpi(win_all,obsd_all%nrecords, &
         obsd_all%great_circle_arc, obsd_all%component_array, &
         ma_weighting_par,weighting_option, &
         rank, comm, ierr)
  print *, "Weighting finished!"

  !init the adjoint source asdf file
  call init_asdf_data(adj_all, obsd_all%nrecords, .false.)

	call MPI_Barrier(comm, ierr)

  allocate(adj_source(measure_adj_par_all%Z%nn))
  !output dir for measure_adj file
  write(p1_string,'(I8)') int(obsd_all%min_period)
  write(p2_string,'(I8)') int(obsd_all%max_period)
  p1_string=adjustl(p1_string)
  p2_string=adjustl(p2_string)
  !output dir for ma subroutine
  ma_outdir=trim(MEASURE_ADJ_OUTDIR)//'/'//trim(synt_all%event)//&
    '_'//trim(p1_string)//'_'//trim(p2_string)
  call system('mkdir -p '//trim(ma_outdir)//'')
  
  print *, trim(ma_outdir)

  !loop over all the records
  do i=1, obsd_all%nrecords
    !call measure_adj subroutine
    call measure_adj(obsd_all%records(i)%record,obsd_all%npoints(i),obsd_all%begin_value(i),obsd_all%sample_rate(i),&
      synt_all%records(i)%record,synt_all%npoints(i),synt_all%begin_value(i),synt_all%sample_rate(i),&
      synt_phydisp_all%records(i)%record,synt_phydisp_all%npoints(i),synt_phydisp_all%begin_value(i),synt_phydisp_all%sample_rate(i),&
      obsd_all%great_circle_arc(i),obsd_all%receiver_name_array(i),obsd_all%network_array(i),obsd_all%component_array(i),&
      win_all(i),measure_adj_par_all, ma_weighting_par, weighting_option,&
      win_chi_all(i), adj_source, ma_outdir)

    !print *,"i, npoints:",i,measure_adj_par_all%Z%nn
		adj_all%npoints(i)=measure_adj_par_all%Z%nn
    adj_all%begin_value(i)=measure_adj_par_all%Z%tt
    adj_all%sample_rate(i)=measure_adj_par_all%Z%dtt
    !print *,"i, npoints:",i,adj_all%npoints(i)
		allocate(adj_all%records(i)%record(adj_all%npoints(i)))
    adj_all%records(i)%record(1:adj_all%npoints(i))=adj_source(1:adj_all%npoints(i))
  end do !enddo nrecords

  !copy other information into adjoint source structure
 	call copy_general_info_to_adj(obsd_all, adj_all)
 	!--------------------------.
 	!write out win_chi_info    !
 	!--------------------------'
 	call write_win_chi(MEASURE_ADJ_OUTDIR, obsd_all%nrecords,&
         obsd_all%event,obsd_all%min_period,obsd_all%max_period,&
         obsd_all%receiver_name_array,obsd_all%network_array,&
         obsd_all%component_array, win_chi_all, win_all,&
         rank, ierr)

	call MPI_Barrier(comm, ierr)

  !--------------------------.
  !rotate                    !
  !--------------------------'
  if(ROTATE_COMP) then
    if(rank.eq.0)then
      print *, "ROTATE:"
    endif
    call rotate_adj(adj_all, adj_all_rotate)
  endif

  !--------------------------.
  !write out                 !
  !--------------------------'
  if(WRITE_ADJ_ASDF) then
  !write out
  !>begin write out the adj_all
    !adios_groupsize = 0.
    ADJ_FILE=trim(MEASURE_ADJ_OUTDIR)//'/'//trim(obsd_all%event)//&
      '_'//trim(p1_string)//'_'//trim(p2_string)//'.bp'
   
    if(rank.eq.0)then
      print *,"------------------"
      print *,"begin write out"
      print *,"ADJ_FILE:",trim(ADJ_FILE)
    endif

    if(ROTATE_COMP) then
      call write_asdf_file (ADJ_FILE, adj_all_rotate, adios_group, &
              rank, nproc, comm, ierr)
    else
      call write_asdf_file (ADJ_FILE, adj_all, adios_group, &
              rank, nproc, comm, ierr)
    endif
  endif

  if(WRITE_ADJ_ASCII) then
    if(rank.eq.0) print *, "Write out normal ascii output file(adj_source)"
  	call write_ascii_output(adj_all, ma_outdir)
    if(ROTATE_COMP)then
      call write_ascii_output(adj_all_rotate, ma_outdir)
    endif
  endif

	!t2=MPI_WTIME()
	!open(unit=22, file='cpu_time')
	!write(22, *) "rank, time:", rank, t2-t1
	!close(22)

end subroutine measure_adj_interface

end module ma_interface
