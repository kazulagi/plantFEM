program main
    use SoybeanClass
    implicit none

    type(Soybean_),allocatable :: SoybeanField(:,:)
    integer :: i,j,itr,pl_id
    character(10) :: id
    real(8) :: iw1,iw2,dp,location(3)
    integer :: iw1_n,iw2_n


    print *, "Crop :: soybean"
    print *, "Input interrow width (cm)  "
    read(*,*) iw1
    print *, "Input intrarow width (cm)  "
    read(*,*) iw2
    print *, "Input number of intrarow   "
    read(*,*) iw1_n
    print *, "Input number of intrarow   "
    read(*,*) iw2_n
    print *, "Input depth (cm)  "
    read(*,*) dp


    allocate(SoybeanField(iw1_n,iw2_n) )

    ! Plant Soybeans on Field
    do i=1,iw1_n
        do j=1,iw2_n
            location(1)=dble(i-1)*iw1
            location(2)=dble(j-1)*iw2
            location(3)=dp
            call SoybeanField(i,j)%sowing(location=location)
        enddo
    enddo

    ! Visualize soybean-field
    itr = 1
    pl_id=0
    do i=1,iw1_n
        do j=1,iw2_n
            pl_id=pl_id+1
            id=trim(  adjustl(fstring( pl_id ) ))
            print *, id
            call SoybeanField(i,j)%export(FileName="/home/haruka/test/seed"//trim(id)//".geo",SeedID=itr)
        enddo
    enddo

end program 