module win_io_subs

	!use asdf_data
  use flexwin_struct
	!use user_parameters
	implicit none

contains

  !-------------------------------------------------------------------	
  subroutine win_write_mt(OUTDIR, event, p1, p2, nrecords, receiver_name, &
                network, component, receiver_id, win, myid)

    integer :: OON=50
    character(len=*) :: OUTDIR
    integer :: nrecords
    character(len=*),intent(in) :: event
    double precision :: p1, p2
    type(win_info),intent(in) :: win(:)
    !type(asdf_event),intent(in) :: obsd_all
    character(len=*) :: receiver_name(:), network(:)
    character(len=*) :: component(:), receiver_id(:)
    integer :: myid

    integer :: i,j
    character(len=120) :: outfile_name
    character(len=10) :: myid_string, p1_string, p2_string

    !OUTDIR="."
    print *, "event:",trim(event)
    !print *,trim(OUTDIR)//'/'//trim(event)
    !call system('mkdir -p '//trim(OUTDIR)//'/'//trim(event)//'')
    write(myid_string,'(i8)')myid
    write(p1_string, '(i8)') int(p1)
    write(p2_string, '(i8)') int(p2)
    myid_string=adjustl(myid_string)
    p1_string=adjustl(p1_string)
    p2_string=adjustl(p2_string)

    outfile_name=trim(OUTDIR)//'/'//trim(event)//'_'//&
              trim(p1_string)//'_'//trim(p2_string)
    print *, "1:", trim(outfile_name)
    call system('mkdir -p '//trim(outfile_name)//'')
    outfile_name=trim(outfile_name)//'/'//trim(myid_string)//'.win.dat'
    !print *, "2:", trim(outfile_name)
    !outfile_name=trim(outdir)//'/'//trim(event)//'_'//&
    !  trim(p1_string)//'_'//trim(p2_string)//'/'&
    !  //trim(myid_string)//'.win.mat'
    print *,trim(outfile_name)

    open(unit=OON,file=outfile_name)
    write(OON,*) nrecords
    do i=1,nrecords
      !write(OON,*)trim(obsd_all%receiver_name(i))//'.'//trim(obsd_all%network(i))//'.'//trim(obsd_all%component(i))
      write(OON,*) trim(receiver_name(i)),'.',trim(network(i)),'.',&
                    trim(component(i)),'.',trim(receiver_id(i))
      write(OON,*)win(i)%num_win
      if(win(i)%num_win.gt.0)then
        do j=1,win(i)%num_win
          write(OON,*) win(i)%t_start(j),win(i)%t_end(j)
        enddo
      endif
    enddo
    close(OON)
  
  end subroutine win_write_mt


  !-------------------------------------------------------------------	
  !write small files. Each file contains one window selection result
  subroutine win_write_single_file(OUTDIR_BASE, event, p1, p2, nrecords, &
    receiver_name, network, component, receiver_id, win, myid)

    integer :: OON=50
    character(len=*) :: OUTDIR_BASE
    integer :: nrecords
    character(len=*),intent(in) :: event
    double precision :: p1, p2
    type(win_info),intent(in) :: win(:)
    !type(asdf_event),intent(in) :: obsd_all
    character(len=*) :: receiver_name(:), network(:)
    character(len=*) :: component(:), receiver_id(:)
    integer :: myid

    integer :: i,j
    character(len=120) :: outfile_dir, outfile_name
    character(len=10) :: myid_string, p1_string, p2_string

    !OUTDIR="."
    print *, "event:",trim(event)
    !print *,trim(OUTDIR)//'/'//trim(event)
    !call system('mkdir -p '//trim(OUTDIR)//'/'//trim(event)//'')
    write(myid_string,'(i8)')myid
    write(p1_string, '(i8)') int(p1)
    write(p2_string, '(i8)') int(p2)
    myid_string=adjustl(myid_string)
    p1_string=adjustl(p1_string)
    p2_string=adjustl(p2_string)

    outfile_dir=trim(OUTDIR_BASE)//'/'//trim(event)//'_'//&
              trim(p1_string)//'_'//trim(p2_string)
    print *, "1:", trim(outfile_dir)
    call system('mkdir -p '//trim(outfile_dir)//'')

    do i=1, nrecords
      component(i)(1:2)="LH"
      outfile_name=trim(outfile_dir)//'/'//trim(receiver_name(i))//&
      '.'//trim(network(i))//'.'//trim(component(i))//&
      '.win.dat'
      !trim(receiver_id(i))//'.win.dat'

      !print *, "2:", trim(outfile_name)
      !outfile_name=trim(outdir)//'/'//trim(event)//'_'//&
      !  trim(p1_string)//'_'//trim(p2_string)//'/'&
      !  //trim(myid_string)//'.win.mat'
      print *,trim(outfile_name)

      open(unit=OON,file=outfile_name)
      !write(OON,*) win(i)%num_win
      !write(OON,*)trim(obsd_all%receiver_name(i))//'.'//trim(obsd_all%network(i))//'.'//trim(obsd_all%component(i))
      !  write(OON,*) trim(receiver_name(i)),'.',trim(network(i)),'.',&
      !              trim(component(i)),'.',trim(receiver_id(i))
      !  write(OON,*)win(i)%num_win
      if(win(i)%num_win.gt.0)then
        do j=1,win(i)%num_win
          write(OON,*) j,win(i)%t_start(j),win(i)%t_end(j)
        enddo
      endif
      close(OON)
    enddo
  
  end subroutine win_write_single_file



  subroutine win_read(FLEXWIN_OUTDIR,event,p1,p2,win_all,&
                        nrecords,rank,ierr)

    character(len=*) :: FLEXWIN_OUTDIR
    real :: p1, p2
    character(len=*) :: event
    type(win_info), allocatable :: win_all(:)
    integer :: nrecords
    integer :: rank ,ierr

    character(len=150) :: dummy, p1_string, p2_string
    character(len=150) :: WIN_FILE
    integer :: i,j
    integer :: num_win
    integer :: IIN=110

    write(dummy,'(I6)') rank
    write(p1_string,'(I6)') int(p1)
    write(p2_string,'(I6)') int(p2)
    dummy=adjustl(dummy)
    p1_string=adjustl(p1_string)
    p2_string=adjustl(p2_string)

    WIN_FILE=trim(FLEXWIN_OUTDIR)//'/'//trim(event)//'_'//&
              trim(p1_string)//'_'//trim(p2_string)
    WIN_FILE=trim(WIN_FILE)//'/'//trim(dummy)//'.win.mat'

    !print *, "inside win_read"
    open(unit=IIN,file=WIN_FILE)
    do i=1, nrecords
      read(IIN, *, iostat=ierr) dummy
      read(IIN, *, iostat=ierr) num_win
      !print *, "i_records=",i,"num_win=",num_win
      win_all(i)%num_win=num_win
      !allocate the t_start and t_end
      allocate(win_all(i)%t_start(num_win))
      allocate(win_all(i)%t_end(num_win))
      do j=1, num_win
        read(IIN,*, iostat=ierr) win_all(i)%t_start(j),win_all(i)%t_end(j)
      enddo
    enddo
    close(IIN)
  end subroutine win_read
	
  subroutine win_write_demo(out_fn,nrecords,win)

    integer :: nrecords
    character(len=150) :: out_fn
    type(win_info),intent(in) :: win(:)

    integer :: OON=50
    integer :: i,j

    open(unit=OON,file=out_fn)
    do i=1,nrecords
      !write(OON,*)trim(obsd_all%receiver_name(i))//'.'//trim(obsd_all%network(i))//'.'//trim(obsd_all%component(i))
      write(OON,*)"receiver:",i
      write(OON,*)win(i)%num_win
      if(win(i)%num_win.gt.0)then
        do j=1,win(i)%num_win
          write(OON,*) win(i)%t_start(j),win(i)%t_end(j)
        enddo
      endif
    enddo
    close(OON)
  end subroutine win_write_demo
  
end module win_io_subs
