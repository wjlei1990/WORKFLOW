module measure_adj_interface_subs

  implicit none

contains

subroutine read_ma_parfile_mpi(ma_par_all, min_period, &
             max_period, event_dpt, nrecords, &
             USE_PHYDISP, rank, comm, ierr)

	use ma_struct
  use measure_adj_subs

  include 'mpif.h'

	type(ma_par_struct_all) :: ma_par_all
	double precision :: min_period, max_period, event_dpt(:)
  logical :: USE_PHYDISP
	integer :: rank, comm, ierr
  integer :: nrecords
  
  double precision :: aver_event_dpt

  integer, parameter :: NDIM_PAR=3
  integer, parameter :: BLOCK_PER_DIM=4
  type(ma_par_struct) :: ma_par_temp(NDIM_PAR)

	integer :: oldtype(BLOCK_PER_DIM), newtype
  integer :: offset(BLOCK_PER_DIM), blockcount(BLOCK_PER_DIM)
	integer :: extent

	integer :: tag=1, i, source, loc
	integer :: stat(MPI_STATUS_SIZE)

	!print *,"SET UP"
	!setup description of the flexwin_par
	!call read_flexwin_parfile_mpi(flexwin_par, fstart, fend, rank, nproc, comm)

  blockcount = (/5,5,19,10/)
  !LOGICAL
  offset(1) = 0
	oldtype(1) = MPI_LOGICAL

  !INTEGER
	call MPI_TYPE_EXTENT(MPI_LOGICAL, extent, ierr)
	offset(2) = offset(1)+blockcount(1)*extent
	oldtype(2) = MPI_INTEGER

  !DOUBLE_PRECISION
	call MPI_TYPE_EXTENT(MPI_INTEGER,extent, ierr)
	offset(3) = offset(2)+blockcount(2)*extent
	oldtype(3) = MPI_DOUBLE_PRECISION

  !CHARACTER
	call MPI_TYPE_EXTENT(MPI_DOUBLE_PRECISION, extent, ierr)
	offset(4) = offset(3)+blockcount(3)*extent
	oldtype(4) = MPI_CHARACTER
	!if(rank.eq.0) then
	!	print *,"blockcount",loc,blockcount(loc)
	!endif
	!if(rank.eq.1) then
	!	print *,"blockcount:",blockcount(loc)
	!endif

	!print *,i, blockcount(1), blockcount(2), blockcount(3), blockcount(4),&
	!					blockcount(5), blockcount(6)

	!now define and commit
	call MPI_TYPE_STRUCT(BLOCK_PER_DIM, blockcount, offset, oldtype, newtype, ierr)
	call MPI_TYPE_COMMIT(newtype, ierr)

  aver_event_dpt=sum(event_dpt(1:nrecords))/nrecords
  if(abs(aver_event_dpt-event_dpt(1)).gt.5.0)then
    print *,"check the data(event_dpt). May not in from one event!"
    stop
  endif
	print *, "SET UP finished!"
	if(rank.eq.0) then
		call read_ma_parfile(ma_par_all,min_period,max_period,aver_event_dpt)
    ma_par_temp(1)=ma_par_all%R
    ma_par_temp(2)=ma_par_all%T
    ma_par_temp(3)=ma_par_all%Z
	endif

  call MPI_Bcast(ma_par_temp, 3, newtype, 0, comm, ierr)

	!if(rank==1) then
	!	print *,"HERE check"
	!	print *, ma_par_temp(:)%TLONG, ma_par_temp(:)%TSHORT
	!endif
	!call MPI_Barrier(comm, ierr)

  ma_par_all%R=ma_par_temp(1)
  ma_par_all%T=ma_par_temp(2)
  ma_par_all%Z=ma_par_temp(3)

  ma_par_all%R%USE_PHYSICAL_DISPERSION=USE_PHYDISP
  ma_par_all%T%USE_PHYSICAL_DISPERSION=USE_PHYDISP
  ma_par_all%Z%USE_PHYSICAL_DISPERSION=USE_PHYDISP

	!if(rank==1) then
		!print *, "CHECK"
		!print *, ma_par_all%Z%WTR
		!print *, ma_par_all%Z%TLONG, ma_par_all%Z%TSHORT
		!print *, trim(ma_par_all%Z%chan)
		!print *, ma_par_all%Z%RUN_BANDPASS
	!endif
  print *, "finalize"

	!stop

end subroutine read_ma_parfile_mpi

subroutine write_win_chi(MEASURE_ADJ_OUTDIR, nrecords, &
              event_name, p1, p2, sta, net,&
              chan_syn, win_chi_all, win_all,&
              rank, ierr)

  use ma_struct
  use flexwin_struct
  implicit none
  
  type(win_chi_info),dimension(:),intent(in) :: win_chi_all
  type(win_info),dimension(:), intent(in) :: win_all
  character(len=*) :: event_name
  double precision :: p1, p2
  character(len=*) :: sta(:),net(:),chan_syn(:)
  integer :: nrecords
  character(len=*) :: MEASURE_ADJ_OUTDIR
  integer :: rank, ierr

  integer :: IIN=110
  integer :: i,j,k,win_index

  character(len=32) :: file_prefix, outdir 
  character(len=250) :: fn, p1_string, p2_string, myid_string

  write(myid_string, '(I8)') rank
  write(p1_string, '(I8)') int(p1)
  write(p2_string, '(I8)') int(p2)
  myid_string=adjustl(myid_string)
  p1_string=adjustl(p1_string)
  p2_string=adjustl(p2_string)
  outdir=MEASURE_ADJ_OUTDIR
  
  call system('mkdir -p '//trim(outdir)//'')
  fn=trim(outdir)//'/'//&
      trim(event_name)//'_'//trim(p1_string)//'_'//&
      trim(p2_string)//'.'//trim(myid_string)//'.winchi'
  open(UNIT=IIN,file=fn)

  do i=1,nrecords
    print *, "nrecords:", nrecords
    !print *,"i",i
    print *, win_all(i)%num_win
    do j=1,win_all(i)%num_win
      !print *, "irecords, num of window:", i,j
      !print *, "sta:", trim(sta(i))
      !print *, "net:", trim(net(i))
      !print *, "comp:", trim(chan_syn(i))
      file_prefix=trim(sta(i))//"."//trim(net(i))//"."//trim(chan_syn(i))
      print *,"file_prefix: ", file_prefix
      write(IIN,'(a14,a8,a3,a5,i4,i4,2e14.6,20e14.6,2e14.6,2f14.6)') &
           trim(file_prefix),trim(sta(i)),trim(net(i)),trim(chan_syn(i)),j,&
           win_chi_all(i)%imeas(j),&
           win_all(i)%t_start(j),win_all(i)%t_end(j),&
           (win_chi_all(i)%chi(j,k),k=1,20),&
           win_chi_all(i)%tr_chi(j),win_chi_all(i)%am_chi(j),&
           win_chi_all(i)%T_pmax_dat(j),win_chi_all(i)%T_pmax_syn(j)
      print *, '   tr_chi = ', sngl(win_chi_all(i)%tr_chi(j)),&
               '   am_chi = ', sngl(win_chi_all(i)%am_chi(j))
    enddo
  enddo

  close(IIN)
  win_index=0
  fn=trim(outdir)//'/'//&
      trim(event_name)//'_'//trim(p1_string)//'_'//&
      trim(p2_string)//'.'//trim(myid_string)//'.winindex'
  open(UNIT=IIN,file=fn)
  do i=1,nrecords
    do j=1,win_all(i)%num_win
      win_index=win_index+1
      write(IIN,'(a3,a8,a5,3i5,2f12.3)') trim(net(i)),trim(sta(i)), &
        trim(chan_syn(i)),win_index, i, j, win_all(i)%t_start(j), &
        win_all(i)%t_end(j)
    enddo
  enddo

  close(IIN)


end subroutine write_win_chi


subroutine write_ascii_output(my_asdf, outdir)

  use asdf_data
  use ascii_rw
  use ma_constants, only : DO_RAY_DENSITY_SOURCE

  type(asdf_event) :: my_asdf
  character(len=150) :: outdir

  double precision, allocatable :: data(:)
  double precision :: b, dt
  integer :: npt

  integer :: i, j

  character(len=300) :: fn, file_prefix

	!do a channel name modify here
	do i=1,my_asdf%nrecords
		my_asdf%component_array(i)(1:2)="LH"
	enddo

  do i=1,my_asdf%nrecords
    file_prefix=trim(my_asdf%receiver_name_array(i))//"."//&
          trim(my_asdf%network_array(i))//"."//&
          trim(my_asdf%component_array(i))
    fn=trim(outdir)//"/"//trim(file_prefix)//".adj"
    print *, "fn:", trim(fn)

    allocate(data(my_asdf%npoints(i)))
    data(:)=dble(my_asdf%records(i)%record)
    b=dble(my_asdf%begin_value(i))
    dt=dble(my_asdf%sample_rate(i))
    npt=my_asdf%npoints(i)
    call dwascii(fn, data, npt, b, dt)
    deallocate(data)
  enddo

end subroutine write_ascii_output

subroutine copy_general_info_to_adj(obsd, adj)
  
  use asdf_data
  implicit none

  type(asdf_event) :: obsd, adj
  integer :: i

  adj%event_lat(:)=obsd%event_lat(:)
  adj%event_lo(:)=obsd%event_lo(:)
  adj%event_dpt(:)=obsd%event_dpt(:)
  adj%event=obsd%event
  adj%min_period=obsd%min_period
  adj%max_period=obsd%max_period

  !adj%receiver_name=""
  !adj%network=""
  !adj%component=""
  !adj%receiver_id=""

  !adj%receiver_name=obsd%receiver_name
  !adj%network=obsd%network
  !adj%component=obsd%component
  !adj%receiver_id=obsd%receiver_id

  !print *, trim(obsd%receiver_id)
  !print *, trim(adj%receiver_id)

  do i=1,obsd%nrecords
    adj%receiver_lat(:)=obsd%receiver_lat(:)
    adj%receiver_lo(:)=obsd%receiver_lo(:)
    adj%scale_factor(:)=obsd%scale_factor(:)

    adj%receiver_name_array(:)=obsd%receiver_name_array(:)
    adj%network_array(:)=obsd%network_array(:)
    adj%component_array(:)=obsd%component_array(:)
    adj%receiver_id_array(:)=obsd%receiver_id_array
    !adj%=obsd%
    !adj%=obsd%
    !adj%=obsd%
  enddo

end subroutine copy_general_info_to_adj

end module measure_adj_interface_subs
