module InsectClass
    use fem
    use LeafClass
    implicit none

    type :: Insect_
        type(Leaf_),pointer :: Leaf
        real(real64) :: x(3) ! position: x, y, z
        real(real64) :: volume ! m^3
        real(real64) :: eatSpeed ! m^3/day
        
    contains
        procedure :: create => createInsect
        procedure :: set => setInsect
        procedure :: eat => eatInsect
    end type

contains

! #################################################
subroutine createInsect(obj,x,y,z,volume,eatSpeed)
    class(Insect_),intent(inout) :: obj
    real(real64),optional,intent(in) :: x,y,z,volume,eatSpeed

    obj%x(1) = input(default=0.0d0, option=x)
    obj%x(2) = input(default=0.0d0, option=y)
    obj%x(3) = input(default=0.0d0, option=z)

    obj%volume = input(default=0.0d0, option=volume)
    obj%eatSpeed = input(default=1.0d0, option=eatSpeed)

end subroutine
! #################################################


! #################################################
subroutine setInsect(obj,leaf)
    class(Insect_),intent(inout) :: obj
    type(Leaf_),target,intent(in) :: leaf

    ! set insect on domains
    if(associated(obj%leaf) )then
        nullify(obj%leaf)
    endif

    obj%leaf => leaf

end subroutine
! #################################################


! #################################################
subroutine eatInsect(obj, dt,debug)
    class(Insect_),intent(inout) :: obj
    real(real64),intent(in) ::  dt
    type(Random_) :: random
    logical,optional,intent(in) ::  debug
    integer(int32) :: i,j,n,elemid,elem_num,nodeid,itr,nodeid_tr,k,old_elemid
    integer(int32),allocatable :: eatElemList(:),elemnod(:,:),neiElemList(:)
    real(real64),allocatable :: x(:)
    real(real64) :: relem,dv,dist,dist_tr

    if(.not. associated(obj%leaf) )then
        print *, "Caution :: InsectClass :: No leaf to eat!!"
        return
    endif




    ! Get a random element

    elem_num = obj%leaf%femdomain%ne()
    relem = dble(elem_num-1) * random%random() + 1.0d0

    elemid = int(relem)
    
    obj%x(:) = obj%leaf%femdomain%mesh%getCenterCoordinate(elemid=elemid)
    
    allocate(eatElemList(elem_num))
    eatElemList(:) = 0

    ! start from elemid
    itr=0

    do 
        itr = itr + 1

        ! try to eat untill full
        dv = obj%leaf%femdomain%getVolume(elemid)
        

        if(obj%volume+dv < dt*obj%eatSpeed)then
            print *,obj%volume,dv
        endif
        
        if(obj%volume+dv > dt*obj%eatSpeed)then
            exit
        else
            eatElemList(elemid) = 1
            obj%volume = obj%volume + dv
            ! move to next element
            neiElemList = obj%leaf%femdomain%mesh%getNeighboringElement(elemid)
            old_elemid = elemid
            do i=1,size(neiElemList)
                ! search neighbor elements
                if( eatElemList( neiElemList(i) ) ==0 )then
                    elemid = neiElemList(i)
                    exit
                endif
            enddo
            if(old_elemid == elemid)then
                ! search over all elements
                do i=1,size(eatElemList)
                    if(eatElemList(i) ==0)then
                        elemid = eatElemList(i)
                        exit
                    endif
                enddo
            endif
            if(old_elemid == elemid)then
                ! all elements are eaten
                exit
            endif

        endif
            
    enddo

    

    do i=size(eatElemList),1,-1
        if(eatElemList(i)==1 )then
            call removeArray(&
                mat=obj%leaf%femdomain%mesh%elemnod,&
                remove1stColumn=.true.,&
                NextOf=i-1 &
                )
        endif
    enddo

   call obj%leaf%femdomain%mesh%clean()

    return


    ! regacy

    ! find the nearest element => not implemented now.
    ! start from a random element

!    elem_num = obj%leaf%femdomain%ne()
!    relem = dble(elem_num-1) * random%random() + 1.0d0
!
!    elemid = int(relem)
!    
!    nodeid = obj%leaf%femdomain%mesh%elemnod(elemid,1) !
!    obj%x(:) = obj%leaf%femdomain%mesh%nodcoord(nodeid,:)
!    
!    allocate(eatElemList(elem_num))
!    eatElemList(:) = 0
!    itr = 0
!    
!    do
!        itr = itr + 1
!        dv = 0.0d0
!        !nodeid = 0
!        if(minval(eatElemList)==1 )then
!            exit
!        endif
!        k=0
!        ! greedy method
!        ! find nearest element
!        do i=1,elem_num
!            k=k+1
!            nodeid_tr = obj%leaf%femdomain%mesh%elemnod(i,1)
!            if(nodeid_tr==nodeid)then
!                cycle
!            endif
!
!            if(k==1)then
!                nodeid_tr = obj%leaf%femdomain%mesh%elemnod(i,1)
!                
!                x(:) = obj%leaf%femdomain%mesh%nodcoord(nodeid_tr,:)
!                dist = sqrt(dot_product(x(1:3)-obj%x(1:3),x-obj%x(1:3) )  )
!                elemid = i
!                cycle
!            endif
!            
!            if(eatElemList(i)==1 )then
!                cycle
!            else
!                nodeid_tr = obj%leaf%femdomain%mesh%elemnod(i,1)
!                x(:) = obj%leaf%femdomain%mesh%nodcoord(nodeid_tr,:)
!                dist_tr = sqrt(dot_product(x(1:3)-obj%x(1:3),x-obj%x(1:3) )  )
!                if(dist_tr < dist)then
!                    dist = dist_tr
!                    elemid = i
!                endif
!            endif
!        enddo
!
!        
!        ! try to eat untill full
!        dv = obj%leaf%femdomain%getVolume(elemid)
!        eatElemList(elemid) = 1
!
!        !print *, itr, dv,nodeid,dist_tr,obj%x(:)
!        !stop
!        !print *, nodeid,"-",nodeid_tr
!        
!        !print *, elemid,sum(eatElemList)
!        !if(itr==3)then
!        !    stop
!        !endif
!
!
!        if(sum(eatElemList) == size(eatElemList) )then
!            print *, "all of the leaf was eaten"
!            exit
!        endif
!
!        if(dv+obj%volume > dt*obj%eatSpeed)then
!            eatElemList(elemid) = 0
!            exit
!        else
!            ! insect eats
!            obj%volume = obj%volume + dv
!            
!            if(present(debug) )then
!                if(debug .eqv. .true.)then
!                    if(mod(itr,100)==0 )then
!                        print *, obj%volume,"/",dt*obj%eatSpeed
!                    endif
!                endif
!            endif
!            eatElemList(elemid) = 1
!
!            !print *, elemid,sum(eatElemList)
!            ! insect moves
!            nodeid = obj%leaf%femdomain%mesh%elemnod(elemid,1)
!            obj%x(:) = obj%leaf%femdomain%mesh%nodcoord(nodeid,:)
!        endif
!        
!        
!    enddo
!
!
!    !print *, itr, obj%volume
!    !print *, sum(eatElemList)
!    
!
!    do i=1,size(eatElemList)
!        if(eatElemList(i)==1 )then
!            call removeArray(&
!                mat=obj%leaf%femdomain%mesh%elemnod,&
!                remove1stColumn=.true.,&
!                NextOf=i-1 &
!                )
!        endif
!    enddo
!
!   call obj%leaf%femdomain%mesh%clean()


end subroutine eatInsect
! #################################################
end module 