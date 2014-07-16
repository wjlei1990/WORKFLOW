module main_subs

  implicit none

contains

subroutine read_main_parfile(rank, comm, ierr)

  use var_main
  use mpi

  integer :: rank, comm, ierr
  
  if(rank.eq.0)then
    print *, "Read in master node:"
    call read_main_parfile_sub(ierr)
  endif

  print *,"Bcast the par..."
  call MPI_Bcast(OBSD_FILE,300,MPI_CHARACTER,0,comm,ierr)
  call MPI_Bcast(SYNT_FILE,300,MPI_CHARACTER,0,comm,ierr)
  call MPI_Bcast(SYNT_PHYDISP_FILE,300,MPI_CHARACTER,0,comm,ierr)

  call MPI_Bcast(MIN_PERIOD, 1, MPI_REAL, 0, comm, ierr)
  call MPI_Bcast(MAX_PERIOD, 1, MPI_REAL, 0, comm, ierr)

  call MPI_Bcast(FLEXWIN_OUTDIR,300,MPI_CHARACTER,0,comm,ierr)
  call MPI_Bcast(WRITE_SINGLE_WIN_FILE,1,MPI_LOGICAL,0,comm,ierr)
  call MPI_Bcast(REMOVE_SW,1,MPI_LOGICAL,0,comm,ierr)

  call MPI_Bcast(MEASURE_ADJ_OUTDIR,150,MPI_CHARACTER,0,comm,ierr)
  call MPI_Bcast(WEIGHTING_OPTION,1,MPI_INTEGER,0,comm,ierr)
  call MPI_Bcast(USE_PHYDISP, 1, MPI_LOGICAL, 0, comm, ierr)
  call MPI_Bcast(WRITE_ADJ_ASDF,1,MPI_LOGICAL,0,comm,ierr)
  call MPI_Bcast(ROTATE_COMP,1,MPI_LOGICAL,0,comm,ierr)
  call MPI_Bcast(WRITE_ADJ_ASCII,1,MPI_LOGICAL,0,comm,ierr)

	!if(rank.eq.1) then
	!	print *, "MPI_staff"
	!	print *, RUN_FLEXWIN, RUN_MEASURE_ADJ, WRITE_ADJ_ASDF,&
	!			ROTATE_COMP, WRITE_NORMAL_OUTPUT
	!		print *, trim(OBSD_FILE), 	
	!   PRINT *, trim(MEASURE_ADJ_OUTDIR)
	!endif

end subroutine read_main_parfile

subroutine read_main_parfile_sub(ierr)

  !read the parfile for the main(some flags)
  use var_main

  integer :: dummy_row
  integer :: ierr
  integer :: IIN=21
  integer :: i

  character(len=30) :: dummy_string

  !print *,"Read main par"
  dummy_row = 4 
  
  open(UNIT=IIN,FILE="PAR_FILE",iostat=ierr)
  if(ierr.ne.0)then
    print *,"Can't find PAR_FILE_MAIN. Stop! "
    stop
  endif

  do i=1,dummy_row
    read(IIN,*)
  enddo

  !print *,"HERE"
	read(IIN,2) dummy_string, OBSD_FILE
	print *, "OBSD_FILE: ", trim(OBSD_FILE)
	read(IIN,2) dummy_string, SYNT_FILE
	print *, "SYNT_FILE: ", trim(SYNT_FILE)
	read(IIN,2) dummy_string, SYNT_PHYDISP_FILE
	print *, "SYNT_PHYDISP_FILE: ", trim(SYNT_PHYDISP_FILE)

  read(IIN,*)
  read(IIN,*)
  read(IIN,5) dummy_string, MIN_PERIOD
  read(IIN,5) dummy_string, MAX_PERIOD
  print *, "min and max period:", MIN_PERIOD, MAX_PERIOD

  read(IIN,*)
  read(IIN,*)
	read(IIN,2) dummy_string, FLEXWIN_OUTDIR 
	print *, "WIN_DIR: ", trim(FLEXWIN_OUTDIR)
  read(IIN,3) dummy_string, WRITE_SINGLE_WIN_FILE
  print *,"ROTATE_COMP: ", WRITE_SINGLE_WIN_FILE
  read(IIN,3) dummy_string, REMOVE_SW
  print *,"WRITE_NORMAL_OUTPUT: ", REMOVE_SW

  read(IIN,*)
  read(IIN,*)
	read(IIN,2) dummy_string, MEASURE_ADJ_OUTDIR
	print *, "MEASURE_ADJ_OUTDIR: ", trim(MEASURE_ADJ_OUTDIR)
  read(IIN,4) dummy_string, weighting_option
  print *, "weighting_option: ", weighting_option
  read(IIN,3) dummy_string, USE_PHYDISP
  print *, "use physical dispersiono:", USE_PHYDISP
  read(IIN,3) dummy_string, WRITE_ADJ_ASDF
  print *,"WRITE_ADJ_ASDF: ", WRITE_ADJ_ASDF
  read(IIN,3) dummy_string, ROTATE_COMP 
  print *,"ROTATE_COMP: ", ROTATE_COMP
  read(IIN,3) dummy_string, WRITE_ADJ_ASCII
  print *,"WRITE_NORMAL_OUTPUT: ",WRITE_ADJ_ASCII

2 format(a,a)
3 format(a,l20)
4 format(a,i)
5 format(a,F15.5)

  close(IIN)
	!stop

end subroutine read_main_parfile_sub

end module main_subs
