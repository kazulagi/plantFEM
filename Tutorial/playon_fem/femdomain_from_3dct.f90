program main
    use FEMDomainClass
    implicit none

    type(FEMDomain_) :: mesh
    type(IO_) :: pixelPosition
    real(real64),allocatable :: position(:,:)
    integer(int32) :: num_layer,i,j,itr,node1,node2,node3,node4,count,prev_node1
    integer(int32), allocatable :: elemnod(:,:)
    integer(int32) :: nearest_node_id,node_id,elist(2),tri_excep,tri_excep_last
    integer(int32),allocatable :: checked(:),checked_node(:)
    real(real64) :: x,y,z
    type(Random_) :: random

    call random%init()
    ! read plant pixels
    num_layer = pixelPosition%numLine("pixelPosition.txt")
    allocate(position(num_layer,3))

    call pixelPosition%open("pixelPosition.txt")
    do i=1,num_layer
        read(pixelPosition%fh,*) position(i,1:3)
    enddo
    call pixelPosition%close()

    !position(:,1) = position(:,1)/10.0d0
    ! convert them into 3D FEMesh

    itr = 0
    mesh%meshtype = "root"
    mesh%mesh%nodcoord = position
    allocate(mesh%mesh%elemnod(size(mesh%mesh%nodcoord,1)*2 ,8) )
    do 
        itr = itr + 1

        if(itr > size(mesh%mesh%nodcoord,1) ) exit
        x = mesh%mesh%nodcoord(itr,1)
        y = mesh%mesh%nodcoord(itr,2)
        z = mesh%mesh%nodcoord(itr,3)
        nearest_node_id = mesh%getNearestNodeID(x=x,y=y,z=z,except=itr)
        mesh%mesh%elemnod(2*itr-1,1) = itr
        mesh%mesh%elemnod(2*itr-1,2:) = nearest_node_id
        elist(1)=itr
        elist(2)=nearest_node_id
        x = mesh%mesh%nodcoord(itr,1)
        y = mesh%mesh%nodcoord(itr,2)
        z = mesh%mesh%nodcoord(itr,3)
        nearest_node_id = mesh%getNearestNodeID(x=x,y=y,z=z,exceptlist=elist)
        mesh%mesh%elemnod(2*itr,1) = itr
        mesh%mesh%elemnod(2*itr,2:) = nearest_node_id
    enddo

    ! remove overlap elements

    ! case 1:
    ! A->A

    itr = 0
    do i=1,size(mesh%mesh%elemnod,1)
        if(mesh%mesh%elemnod(i,1) == mesh%mesh%elemnod(i,2))then
            mesh%mesh%elemnod(i,:) = 0
            itr=itr+1
        endif
    enddo

    ! A -> B
    ! B <- A
    do i=1,size(mesh%mesh%elemnod,1)
        if(mesh%mesh%elemnod(i,1)==0 )then
            cycle
        endif
        node1 = mesh%mesh%elemnod(i,1)
        node2 = mesh%mesh%elemnod(i,2)
        do j=i+1,size(mesh%mesh%elemnod,1)
            if(mesh%mesh%elemnod(j,1)==0 )then
                cycle
            endif
            if(mesh%mesh%elemnod(j,1) == node1 .and. &
                mesh%mesh%elemnod(j,2) == node2)then
                mesh%mesh%elemnod(j,:)=0
                itr=itr+1
            endif
            if(mesh%mesh%elemnod(j,1) == node2 .and. &
                mesh%mesh%elemnod(j,2) == node1)then
                mesh%mesh%elemnod(j,:)=0
                itr=itr+1
            endif
        enddo
    enddo
    

    ! case 2
    ! D->A
    ! A->B
    ! B->C
    ! C->A
    ! >> triangle-exception
    ! Find cyclic graph
    
!    node1 = 2
!    node2 = 1
!    node3 = 1
!    node4 = 1
!    allocate(checked(size(mesh%mesh%elemnod,1)) )
!    allocate(checked_node(size(mesh%mesh%nodcoord,1)) )
!    checked(:) = 0
!    checked_node(:) = 0
!    count=0
!    do 
!
!        prev_node1=node1
!        ! triangle-exception探索
!        ! 通った要素はchecked=1
!        tri_excep=0
!
!        do i=1,size(mesh%mesh%elemnod,1)
!            
!            if(checked(i) == 1) then
!                cycle
!            endif
!
!            if(mesh%mesh%elemnod(i,1) == 0 )then
!                cycle
!            endif
!
!            checked(i) = 1
!
!            checked_node(mesh%mesh%elemnod(i,1))=1 
!            checked_node(mesh%mesh%elemnod(i,2))=1
!            
!            ! Find next node >> append
!            if(mesh%mesh%elemnod(i,1) == node1 .and. &
!                mesh%mesh%elemnod(i,2) /= node2)then
!                node4 = node3
!                node3 = node2
!                node2 = node1 
!                node1 = mesh%mesh%elemnod(i,2)
!                checked_node(node1) = 1
!                checked_node(node2) = 1
!                checked_node(node3) = 1
!                checked_node(node4) = 1
!                tri_excep=i
!                checked(i) = 1
!                exit
!            elseif(mesh%mesh%elemnod(i,2) == node1 .and. &
!                mesh%mesh%elemnod(i,1) /= node2)then
!                node4 = node3
!                node3 = node2
!                node2 = node1 
!                node1 = mesh%mesh%elemnod(i,1)
!                checked_node(node1) = 1
!                checked_node(node2) = 1
!                checked_node(node3) = 1
!                checked_node(node4) = 1
!                tri_excep=i
!                checked(i) = 1
!                exit
!            else
!                cycle
!            endif
!
!        enddo
!
!
!        print *, node1, node2,node3,node4
!    
!        print *, countif(Array=checked,Equal=.true.,value=0),"/",size(checked)
!
!
!        if(prev_node1 == node1) then
!            exit
!        endif
!
!        if(countif(Array=checked,Equal=.true.,value=0) ==0  ) then
!            exit
!        endif
!
!
!        if(tri_excep==0)then
!            do j=1,size(checked)
!                if(checked(j)==0 .and. mesh%mesh%elemnod(j,1)/=0 )then
!                    if(checked_node(mesh%mesh%elemnod(j,1 ))==0 )then
!                        node1 = mesh%mesh%elemnod(j,1 )
!                        
!                        exit
!                    elseif(checked_node(mesh%mesh%elemnod(j,2 ))==0 )then
!                        node1 = mesh%mesh%elemnod(j,2 )
!                        exit
!                    else
!                        cycle
!                    endif
!                endif
!            enddo
!        endif
!
!        if(node1==node4 .and. tri_excep/=0)then
!            itr=itr+1
!            ! triangle-exception
!            mesh%mesh%elemnod(tri_excep,:) = 0
!            ! checkされてない中で最も接点番号が若いものから再スタート
!            do j=1,size(checked)
!                if(checked(j)==0 .and. mesh%mesh%elemnod(j,1)/=0 )then
!                    if(checked_node(mesh%mesh%elemnod(j,1 ))==0 )then
!                        node1 = mesh%mesh%elemnod(j,1 )
!                        
!                        exit
!                    elseif(checked_node(mesh%mesh%elemnod(j,2 ))==0 )then
!                        node1 = mesh%mesh%elemnod(j,2 )
!                        exit
!                    else
!                        cycle
!                    endif
!                    cycle
!                endif
!            enddo
!        endif
!
!
!    enddo
!
!    do i=1,size(mesh%mesh%elemnod,1)
!        node1 = mesh%mesh%elemnod(i,1)
!        node2 = mesh%mesh%elemnod(i,2)
!        do j=i+1,size(mesh%mesh%elemnod,1)
!            if(mesh%mesh%elemnod(j,1) == node1 )then
!                node3 = mesh%mesh%elemnod(j,2)
!            endif
!            if(mesh%mesh%elemnod(j,1) == node2 )then
!                node3 = mesh%mesh%elemnod(j,2)
!            endif
!        enddo
!    enddo
!
!

    elemnod = mesh%mesh%elemnod
    deallocate(mesh%mesh%elemnod)
    allocate(mesh%mesh%elemnod(size(elemnod,1)-itr,8 ) )
    mesh%mesh%elemnod(:,:)=0
    itr=0
    do i=1,size(elemnod,1)
        if(minval(elemnod(i,:))==0 )then
            cycle
        else
            itr=itr+1
            mesh%mesh%elemnod(itr,:) = elemnod(i,:)
        endif
    enddo







    call mesh%msh("plant")
end program main
