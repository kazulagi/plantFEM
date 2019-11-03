program main
    use SoybeanClass
    implicit none

    type(Soybean_),allocatable :: SoybeanField(:,:)
    integer :: i,j,itr
    character(10) :: id

    allocate(SoybeanField(1,1) )

    ! Plant Soybeans on Field
    do i=1,1
        do j=1,1
            call SoybeanField(i,j)%sowing(x=dble(i),y=dble(j) )
        enddo
    enddo

    ! Visualize soybean-field
    itr = 0
    do i=1,1
        do j=1,1
            itr=itr+1
            id=trim(  adjustl(fstring( itr ) ))
            print *, id
            !call SoybeanField(i,j)%export(FileName="/home/haruka/test/seed"//trim(id)//".geo",SeedID=itr )
        enddo
    enddo

    ! Grow soybean over dt (sec.)
    !itr = 0
    !do i=1,1
    !    do j=1,1
    !        itr=itr+1
    !        id=trim(  adjustl(fstring( itr ) ))
    !        print *, id
    !        call SoybeanField(i,j)%grow(dt=60.0d0)
    !    enddo
    !enddo

end program 