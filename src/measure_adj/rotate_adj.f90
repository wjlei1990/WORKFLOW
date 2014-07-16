module rotate_subs

implicit none

contains

  subroutine rotate_adj(adj_all, adj_all_rotate)

    type(asdf_event) :: adj_all, adj_all_rotate
    
    integer :: i,j,k,total_num_z
    integer :: num_z

    logical :: t_exist, r_exist

    real ::  azm,bzm,ddg,dkm
    real ::  costh, sinth

    real, allocatable :: zdata(:), rdata(:), tdata(:), ndata(:), edata(:)

    total_num_z=0
    !first see how many z components in the adj_all
    do i=1, adj_all%nrecords
      if(adj_all%component_array(i)(3:3)=="Z") total_num_z = total_num_z+1
    enddo

    call init_asdf_data(adj_all_rotate,3*total_num_z)

    !recalculate the az and baz
    do i=1, adj_all%nrecords
      call distaz(adj_all%event_lat(i), adj_all%event_lo(i), &
                    adj_all%receiver_lat(i), adj_all%receiver_lo(i),
                    azm, bzm, ddg, dkm)
      adj_all%ev_to_sta_AZ(i)=azm
      adj_all%sta_to_ev_AZ(i)=bzm
    enddo

    !loop over to rotate
    order_z=0
    do i=1, adj_all%nrecords
      if(adj_all%component_array(i)(3:3)=="Z") then
        r_exist=.false.
        t_exist=.false.
        order_z=order_z+1
        !find R with same station name
        do j=1, adj_all%nrecords
          if(adj_all%component_array(j)(3:3)=="R" .and. &
            adj_all%receiver_name_array(j)==adj_all%receiver_name_array(i)) then
            r_exist=.true.
            exit
          endif
        enddo
        !find T with same station name
        do k=1, adj_all%nrecords
          if(adj_all%component_array(j)(3:3)=="T" .and. &
            adj_all%receiver_name_array(j)==adj_all%receiver_name_array(i)) 
            t_exist=.true.
            exit
          endif
        enddo

        nn=adj_all%npoints(i)
        bzm=adj_all%sta_to_ev_AZ(i)
        tt=adj_all%begin_value(i)
        dtt=adj_all%sample_rate(i)
        !E,N,Z
        allocate(zdata(nn))
        allocate(rdata(nn))
        allocate(tdata(nn))
        allocate(edata(nn))
        allocate(ndata(nn))
        allocate(adj_all_rotate%records(3*loc_z-2)%record,nn)
        allocate(adj_all_rotate%records(3*loc_z-1)%record,nn)
        allocate(adj_all_rotate%records(3*loc_z)%record,nn)
        if(r_exist.and.t_exist) then
          !rotates
          zdata(1:nn) = adj_all%records(i)%record(1:nn)
          rdata(1:nn) = adj_all%records(j)%record(1:nn)
          tdata(1:nn) = adj_all%records(k)%record(1:nn)
          costh = cos(baz)
          sinth = sin(baz)
          edata(1:nn) = -costh * tdata(1:nn) - sinth * rdata(1:nn)
          ndata(1:nn) =  sinth * tdata(1:nn) - costh * rdata(1:nn)
          adj_all_rotate%records(3*loc_z-2)%record(1:nn)= edata(1:nn)
          adj_all_rotate%records(3*loc_z-1)%record(1:nn)= ndata(1:nn)
          adj_all_rotate%records(3*loc_z)%record(1:nn)  = zdata(1:nn)
        else
          !just write Z and set RT to zero
          zdata(1:nn) = adj_all%records(i)%record(1:nn)
          rdata(1:nn) = 0.
          tdata(1:nn) = 0.
          adj_all_rotate%records(3*loc_z-2)%record(1:nn)= edata(1:nn)
          adj_all_rotate%records(3*loc_z-1)%record(1:nn)= ndata(1:nn)
          adj_all_rotate%records(3*loc_z)%record(1:nn)  = zdata(1:nn)
        endif

        !fill other info
        adj_all_rotate%component_array(3*loc_z-2) = "LHE"
        adj_all_rotate%component_array(3*loc_z-1) = "LHN"
        adj_all_rotate%component_array(3*loc_z) = "LHZ"

        adj_all_rotate%npoints(3*loc_z-2) = nn
        adj_all_rotate%npoints(3*loc_z-1) = nn
        adj_all_rotate%npoints(3*loc_z) = nn

        adj_all_rotate%sample_rate(3*loc_z-2) = dtt
        adj_all_rotate%sample_rate(3*loc_z-1) = dtt
        adj_all_rotate%sample_rate(3*loc_z) = dtt
        
        adj_all_rotate%begin_value(3*loc_z-2) = tt
        adj_all_rotate%begin_value(3*loc_z-1) = tt
        adj_all_rotate%begin_value(3*loc_z) = tt

        adj_all_rotate%receiver_name_array(3*loc_z-2) = &
            adj_all%receiver_name_array(i)
        adj_all_rotate%receiver_name_array(3*loc_z-1) = &
            adj_all%receiver_name_array(i)
        adj_all_rotate%receiver_name_array(3*loc_z) = &
            adj_all%receiver_name_array(i)

        adj_all_rotate%network_array(3*loc_z-2) = &
            adj_all%network_array(i)
        adj_all_rotate%network_array(3*loc_z-1) = &
            adj_all%network_array(i)
        adj_all_rotate%network_array(3*loc_z) = &
            adj_all%network_array(i)

        !adj_all_rotate%(3*loc_z-2) = 
        !adj_all_rotate%(3*loc_z-1) = 
        !adj_all_rotate%(3*loc_z) = 
      endif !if comp==Z
    enddo
    
  end subroutine rotate_adj
        

    
