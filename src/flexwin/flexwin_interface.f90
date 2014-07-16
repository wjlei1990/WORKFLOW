module fw_interface

contains
!> @file
!! Program: Global_Tomography_Data_Processing
!! Developer: Princeton Global Tomography Group(PGTG)
!! Group Member: Wenjie Lei(lei@princeton.edu), Ebru Bozdag(bozdag@princeton.edu),
!! James A. Smith(jas11@princeton.edu)
!! Bug Report: lei@princeton.edu

subroutine flexwin_interface(obsd_all, synt_all, win_all, &
  rank, nproc, comm, ierr)

  use asdf_data
  use flexwin_struct
  use var_main

  use win_io_subs

  use flexwin_subs
	use flexwin_interface_subs
  
  use mpi
  implicit none

  type(asdf_event)        :: synt_all, obsd_all
  type(win_info),allocatable      :: win_all(:)
  type(flexwin_par_struct_all) 		:: flexwin_par_all

  integer :: nrecords

  !mpi_var
  integer                 :: nproc,comm,rank
  integer                 :: ierr,adios_err

  integer                 :: i
  double precision        :: t1, t2, t3, t4

	!t1 = MPI_Wtime()
  if(rank.eq.0) print *, "Start FLEXWIN...NPROC:", NPROC

  !--------------------------.
  !read parfile              !
  !--------------------------'
  call read_flexwin_parfile_mpi(flexwin_par_all, obsd_all%min_period,&
        obsd_all%max_period, obsd_all%event_dpt, obsd_all%nrecords, &
        rank, comm, ierr)

	call MPI_Barrier(comm,ierr)

  allocate(win_all(obsd_all%nrecords))

  !--------------------------.
  !FLEXWIN                   !
  !--------------------------'
	if(rank.eq.0) then
   	print *,"-----------------"
   	print *,"RUNNING FLEXWIN"
   	print *,"-----------------"
	endif

  do i=1, obsd_all%nrecords
print *, "***************"
print *, obsd_all%sample_rate(i)
print *, synt_all%sample_rate(i)
print *, "***************"
   !call flexwin subroutine
     call flexwin(obsd_all%records(i)%record,obsd_all%npoints(i),obsd_all%sample_rate(i),obsd_all%begin_value(i),&
				synt_all%records(i)%record,synt_all%npoints(i),synt_all%sample_rate(i),synt_all%begin_value(i),&
     		obsd_all%event_lat(i), obsd_all%event_lo(i), obsd_all%event_dpt(i),&
				obsd_all%receiver_lat(i),obsd_all%receiver_lo(i),&
     		obsd_all%receiver_name_array(i),obsd_all%network_array(i),obsd_all%component_array(i),&
				synt_all%P_pick(i),synt_all%S_pick(i), synt_all%event,&
     		flexwin_par_all,win_all(i), REMOVE_SW )
  enddo

  call MPI_Barrier(comm, ierr)

  !write out window info
  if(rank.eq.0) print *, "Write out WIN"
  call win_write_mt(FLEXWIN_OUTDIR, obsd_all%event, obsd_all%min_period,&
        obsd_all%max_period, obsd_all%nrecords,&
        obsd_all%receiver_name_array, obsd_all%network_array,&
        obsd_all%component_array, obsd_all%receiver_id_array,&
        win_all, rank)
  if(WRITE_SINGLE_WIN_FILE) then
    call win_write_single_file(FLEXWIN_OUTDIR, obsd_all%event, obsd_all%min_period,&
          obsd_all%max_period, obsd_all%nrecords,&
          obsd_all%receiver_name_array, obsd_all%network_array,&
          obsd_all%component_array, obsd_all%receiver_id_array,&
          win_all, rank)
  endif

	!t2 = MPI_Wtime()
	!open(unit=22, file='cpu_time')
	!write(22, *) "rank, time:", rank, t2-t1
	!close(22)

end subroutine flexwin_interface

end module fw_interface
