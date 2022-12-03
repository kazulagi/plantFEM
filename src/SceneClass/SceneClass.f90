

module SceneClass
    use FEMDomainClass
    use SoybeanClass
    implicit none

    type :: Scene_
        type(FEMDomainp_),allocatable :: femdomain(:)
        type(Soybeanp_),allocatable :: soybean(:)
    contains
        procedure,pass :: add_FEMDomain_to_Scene
        procedure,pass :: add_soybean_to_Scene
        generic :: add =>add_FEMDomain_to_Scene, add_soybean_to_Scene
        procedure,public :: vtk => vtk_FEMDomain_from_Scene
    end type
contains

subroutine add_FEMDomain_to_Scene(this,femdomain)
    class(Scene_),intent(inout) :: this
    type(FEMDomain_),target,intent(in) :: femdomain(:)
    type(FEMDomainp_),allocatable ::  buf(:)
    integer(int32) :: i,n,old_n

    n = size(femdomain)
    old_n = size(this%femdomain)
    if(.not.allocated(this%femdomain) )then
        allocate(this%femdomain(n) )
        do i=1,n
            this%femdomain(i)%femdomainp => femdomain(i)
        enddo
    else
        buf = this%femdomain
        deallocate(this%femdomain)
        allocate(this%femdomain(old_n+n) )
        this%femdomain(1:old_n) = buf(1:old_n)
        do i=1,n
            this%femdomain(i+old_n)%femdomainp => femdomain(i)
        enddo
    endif

end subroutine

! #######################################################


subroutine add_soybean_to_Scene(this,soybean)
    class(Scene_),intent(inout) :: this
    type(soybean_),target,intent(in) :: soybean(:)
    type(soybeanp_),allocatable ::  buf(:)
    integer(int32) :: i,n,old_n

    n = size(soybean)
    old_n = size(this%soybean)
    if(.not.allocated(this%soybean) )then
        allocate(this%soybean(n) )
        do i=1,n
            this%soybean(i)%soybeanp => soybean(i)
        enddo
    else
        buf = this%soybean
        deallocate(this%soybean)
        allocate(this%soybean(old_n+n) )
        this%soybean(1:old_n) = buf(1:old_n)
        do i=1,n
            this%soybean(i+old_n)%soybeanp => soybean(i)
        enddo
    endif

end subroutine

! #######################################################

subroutine vtk_FEMDomain_from_Scene(this,name)
    class(Scene_),intent(inout) :: this
    character(*),intent(in) :: name
    integer(int32) :: i

    if(allocated(this%femdomain))then
        do i=1,size(this%femdomain)
            call this%femdomain(i)%femdomainp%vtk(name+"/femdomain_"+zfill(i,6))  
        enddo
    endif

    if(allocated(this%soybean))then
        do i=1,size(this%soybean)
            call this%soybean(i)%soybeanp%vtk(name+"/soybean_"+zfill(i,6),single_file=.true.)  
        enddo
    endif

end subroutine

end module

