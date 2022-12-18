

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
        generic :: add => add_FEMDomain_to_Scene, add_soybean_to_Scene
        procedure,public :: vtk => vtk_FEMDomain_from_Scene
        
        procedure,public :: nn => nn_Scene
        procedure,public :: ne => ne_Scene

        procedure,public :: x => x_Scene
        procedure,public :: y => y_Scene
        procedure,public :: z => z_Scene
        procedure,public :: xyz => xyz_Scene

        ! info getter
        procedure,public :: xmin => xmin_Scene
        procedure,public :: xmax => xmax_Scene
        procedure,public :: x_min => xmin_Scene
        procedure,public :: x_max => xmax_Scene


        procedure,public :: ymin  => ymin_Scene
        procedure,public :: ymax  => ymax_Scene
        procedure,public :: y_min => ymin_Scene
        procedure,public :: y_max => ymax_Scene

        procedure,public :: zmin  => zmin_Scene
        procedure,public :: zmax  => zmax_Scene
        procedure,public :: z_min => zmin_Scene
        procedure,public :: z_max => zmax_Scene
        ! editor
        procedure,public :: resize => resize_scene
        procedure,public :: rotate => rotate_scene
        procedure,public :: move => move_scene

        procedure,public :: overset => overset_scene
        procedure,public :: getOverlapList => getOverlapList_Scene
        procedure,public :: select_point_ID => select_point_ID_Scene

        procedure,pass   :: full_function_Scene
        procedure,pass   :: full_select_node_in_range_Scene
        generic :: full => full_function_Scene,full_select_node_in_range_Scene

    


    end type


    interface operator(.contacts.)
        module procedure femdomain_contacts_with_femdomain
    end interface


    

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
function nn_Scene(this) result(ret)
    class(Scene_),intent(in) :: this
    integer(int32) :: offset, DomainID, ret

    ret = 0
    do DomainID=1,size(this%femdomain)
        ret = ret + this%femdomain(DomainID)%femdomainp%nn()
    enddo

end function
! #######################################################
function ne_Scene(this) result(ret)
    class(Scene_),intent(in) :: this
    integer(int32) :: offset, DomainID, ret

    ret = 0
    do DomainID=1,size(this%femdomain)
        ret = ret + this%femdomain(DomainID)%femdomainp%ne()
    enddo

end function
! #######################################################
function x_Scene(this,node_id) result(ret)
    class(Scene_),intent(in) :: this
    real(real64),allocatable :: ret(:)
    integer(int32),optional,intent(in) :: node_id
    integer(int32) :: offset, DomainID

    ret = zeros(this%nn() )
    offset = 0.0d0
    do domainID=1,size(this%femdomain)
        ret(offset+1:offset+this%femdomain(DomainID)%femdomainp%nn() ) &
            = this%femdomain(DomainID)%femdomainp%mesh%nodcoord(:,1)
        offset = offset + this%femdomain(DomainID)%femdomainp%nn()
    enddo

    if(present(node_id) )then
        ret = ret(node_id)
    endif

end function
! #######################################################
recursive function y_Scene(this,node_id) result(ret)
    class(Scene_),intent(in) :: this
    real(real64),allocatable :: ret(:)
    integer(int32),optional,intent(in) :: node_id
    integer(int32) :: offset, DomainID

    offset = 0.0d0
    ret = zeros(this%nn() )
    do domainID=1,size(this%femdomain)
        ret(offset+1:offset+this%femdomain(DomainID)%femdomainp%nn() ) &
            = this%femdomain(DomainID)%femdomainp%mesh%nodcoord(:,2)
        offset = offset + this%femdomain(DomainID)%femdomainp%nn()
    enddo
    if(present(node_id) )then
        ret = ret(node_id)
    endif

end function
! #######################################################
recursive function z_Scene(this,node_id) result(ret)
    class(Scene_),intent(in) :: this
    real(real64),allocatable :: ret(:)
    integer(int32),optional,intent(in) :: node_id
    integer(int32) :: offset, DomainID
    offset = 0.0d0
    ret = zeros(this%nn() )
    do domainID=1,size(this%femdomain)
        ret(offset+1:offset+this%femdomain(DomainID)%femdomainp%nn() ) &
            = this%femdomain(DomainID)%femdomainp%mesh%nodcoord(:,3)
        offset = offset + this%femdomain(DomainID)%femdomainp%nn()
    enddo
    if(present(node_id) )then
        ret = ret(node_id)
    endif

end function
! #######################################################


! #######################################################
function xyz_Scene(this) result(ret)
    class(Scene_),intent(in) :: this
    real(real64),allocatable :: ret(:,:)
    integer(int32) :: offset, DomainID

    ret = this%x() .h. this%y() .h. this%z()


end function
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

subroutine vtk_FEMDomain_from_Scene(this,name,scalar)
    class(Scene_),intent(inout) :: this
    character(*),intent(in) :: name
    integer(int32) :: i,from,to
    real(real64),optional,intent(in) :: scalar(:)

    from = 0
    to   = 0

    if(present(scalar) )then
        if(allocated(this%femdomain))then
            do i=1,size(this%femdomain)
                from = to + 1
                to   = from + this%femdomain(i)%femdomainp%nn() - 1
                call this%femdomain(i)%femdomainp%vtk(name+"/femdomain_"+zfill(i,6),scalar=scalar(from:to))
            enddo
        endif
    else
        if(allocated(this%femdomain))then
            do i=1,size(this%femdomain)
                call this%femdomain(i)%femdomainp%vtk(name+"/femdomain_"+zfill(i,6))  
            enddo
        endif
    endif

    if(allocated(this%soybean))then
        do i=1,size(this%soybean)
            call this%soybean(i)%soybeanp%vtk(name+"/soybean_"+zfill(i,6),single_file=.true.)  
        enddo
    endif

end subroutine

! #####################################################
subroutine resize_scene(this,object,x,y,z,x_rate,y_rate,z_rate)
    class(Scene_),intent(inout) :: this
    integer(int32),intent(in) :: object(:)
    real(real64),optional,intent(in) :: x,y,z,x_rate,y_rate,z_rate
    integer(int32) :: domainID,itr
    ! resize femdomain object
    do itr=1,size(object)
        domainID = object(itr)
        call this%femdomain(DomainID)%femdomainp%resize(x_len=x,y_len=y,z_len=z,&
            x_rate=x_rate,y_rate=y_rate,z_rate=z_rate)
    enddo
end subroutine
! #####################################################







! #####################################################
subroutine move_scene(this,object,x,y,z)
    class(Scene_),intent(inout) :: this
    integer(int32),intent(in) :: object(:)
    real(real64),optional,intent(in) :: x,y,z
    integer(int32) :: domainID,itr
    ! move femdomain object
    do itr=1,size(object)
        domainID = object(itr)
        call this%femdomain(DomainID)%femdomainp%move(x=x,y=y,z=z)
    enddo
end subroutine
! #####################################################


! #####################################################
subroutine rotate_scene(this,object,x,y,z)
    class(Scene_),intent(inout) :: this
    integer(int32),intent(in) :: object(:)
    real(real64),optional,intent(in) :: x,y,z
    integer(int32) :: domainID,itr
    
    ! rotate femdomain object
    do itr=1,size(object)
        domainID = object(itr)
        call this%femdomain(DomainID)%femdomainp%rotate(x=x,y=y,z=z)
    enddo

end subroutine
! #####################################################


function getOverlapList_Scene(this) result(Overlap)
    class(Scene_),intent(in) :: this
    integer(int32),allocatable::Overlap(:,:)
    logical,allocatable :: contact_table(:,:)
    integer(int32) :: i,j,n,itr
    
    n = size(this%femdomain)
    allocate(contact_table(n,n) )
    contact_table(:,:) = .false.

    itr = 0
    do i=1,size(this%femdomain)
        do j=1,i-1
            contact_table(i,j) = this%femdomain(i)%femdomainp .contacts. this%femdomain(j)%femdomainp
            if(contact_table(i,j) )then
                itr = itr + 1
            endif
        enddo
    enddo

    allocate(Overlap(itr,2))
    itr = 0
    do i=1,size(this%femdomain)
        do j=1,i-1
            if(contact_table(i,j) )then
                itr = itr + 1
                overlap(itr,1:2) = [i,j]
            endif
        enddo
    enddo





end function

! #####################################################
subroutine overset_scene(this,pairing,x,y,z,algorithm)
    class(Scene_),intent(inout) :: this
    integer(int32),optional,intent(in) :: pairing(1:2)
    real(real64),optional,intent(in) :: x,y,z
    character(*),optional,intent(in) :: algorithm
    integer(int32) :: domainID,itr,i
    integer(int32),allocatable :: pairings(:,:)

    character(:),allocatable :: algol

    if(present(algorithm))then
        algol=algorithm
    else
        algol="GPP"
    endif

    if(present(pairing) )then
        ! move femdomain pairing
        call this%femdomain(pairing(1))%overset(femdomainp=this%femdomain,to=pairing(2),by=algol)
        call this%femdomain(pairing(2))%overset(femdomainp=this%femdomain,to=pairing(1),by=algol)
    else
        pairings = this%getOverlapList()
        do i=1,size(pairings,1)
            call this%femdomain(pairings(i,1))%overset(femdomainp=this%femdomain,to=pairings(i,2),by=algol)
            call this%femdomain(pairings(i,2))%overset(femdomainp=this%femdomain,to=pairings(i,1),by=algol)
        enddo
    endif

end subroutine
! #####################################################
function full_select_node_in_range_Scene(this,domainIDs,values,x_min,x_max,y_min,y_max,z_min,z_max) result(ret)
    class(Scene_),intent(inout) :: this
    integer(int32),intent(in) :: DomainIDs(:)
    real(real64),intent(in) :: values
    real(real64),optional,intent(in) :: x_min,x_max,y_min,y_max,z_min,z_max
    integer(int32) :: itr,DomainID,offset
    integer(int32),allocatable :: num_node_per_domain(:)
    real(real64),allocatable :: ret(:)

    allocate(ret(0) )
    allocate(num_node_per_domain(0:size(this%femdomain)))
    num_node_per_domain(0)=0
    do itr=1,size(this%femdomain)
        num_node_per_domain(itr) = this%femdomain(itr)%femdomainp%nn()
    enddo

    do itr=1,size(domainIDs)
        domainID = domainIDs(itr)
        ret = ret // values*eyes(size(this%femdomain(domainID)%femdomainp%select(&
            x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max) ))
    enddo
    
end function

! #####################################################




function select_point_ID_Scene(this,domainIDs,x_min,x_max,y_min,y_max,z_min,z_max) result(ret)
    class(Scene_),intent(inout) :: this
    integer(int32),intent(in) :: DomainIDs(:)
    real(real64),optional,intent(in) :: x_min,x_max,y_min,y_max,z_min,z_max
    integer(int32) :: itr,DomainID,offset
    integer(int32),allocatable :: ret(:),num_node_per_domain(:)

    allocate(ret(0) )
    allocate(num_node_per_domain(0:size(this%femdomain)))
    num_node_per_domain(0)=0
    do itr=1,size(this%femdomain)
        num_node_per_domain(itr) = this%femdomain(itr)%femdomainp%nn()
    enddo

    do itr=1,size(domainIDs)
        domainID = domainIDs(itr)
        offset   = sum(num_node_per_domain(0:domainID-1))
        ret = ret // this%femdomain(domainID)%femdomainp%select(&
            x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max) &
            + offset
    enddo
    
end function

! #####################################################

function xmax_Scene(this,domainIDs) result(ret)
    class(Scene_),intent(inout) :: this
    integer(int32),optional,intent(in) :: DomainIDs(:)
    real(real64),allocatable :: ret
    integer(int32) :: i
    real(real64),allocatable :: maxvalues(:)

    
    if(present(domainIDs) )then
        maxvalues = zeros(size(domainIDs) )
        do i=1,size(domainIDs)
            maxvalues(i) = this%femdomain(domainIDs(i))%femdomainp%xmax()
        enddo
    else
        maxvalues = zeros( size(this%femdomain) )
        do i=1,size(this%femdomain)
            maxvalues(i) = this%femdomain(i)%femdomainp%xmax()
        enddo
    endif
    ret = maxval(maxvalues)

end function

! #####################################################

function xmin_Scene(this,domainIDs) result(ret)
    class(Scene_),intent(inout) :: this
    integer(int32),optional,intent(in) :: DomainIDs(:)
    real(real64),allocatable :: ret
    integer(int32) :: i
    real(real64),allocatable :: minvalues(:)

    
    if(present(domainIDs) )then
        minvalues = zeros(size(domainIDs) )
        do i=1,size(domainIDs)
            minvalues(i) = this%femdomain(domainIDs(i))%femdomainp%xmin()
        enddo
    else
        minvalues = zeros( size(this%femdomain) )
        do i=1,size(this%femdomain)
            minvalues(i) = this%femdomain(i)%femdomainp%xmin()
        enddo
    endif
    ret = minval(minvalues)

end function





! #####################################################

function ymax_Scene(this,domainIDs) result(ret)
    class(Scene_),intent(inout) :: this
    integer(int32),optional,intent(in) :: DomainIDs(:)
    real(real64),allocatable :: ret
    integer(int32) :: i
    real(real64),allocatable :: maxvalues(:)

    
    if(present(domainIDs) )then
        maxvalues = zeros(size(domainIDs) )
        do i=1,size(domainIDs)
            maxvalues(i) = this%femdomain(domainIDs(i))%femdomainp%ymax()
        enddo
    else
        maxvalues = zeros( size(this%femdomain) )
        do i=1,size(this%femdomain)
            maxvalues(i) = this%femdomain(i)%femdomainp%ymax()
        enddo
    endif
    ret = maxval(maxvalues)

end function
! #####################################################







! #####################################################
function ymin_Scene(this,domainIDs) result(ret)
    class(Scene_),intent(inout) :: this
    integer(int32),optional,intent(in) :: DomainIDs(:)
    real(real64),allocatable :: ret
    integer(int32) :: i
    real(real64),allocatable :: minvalues(:)

    
    if(present(domainIDs) )then
        minvalues = zeros(size(domainIDs) )
        do i=1,size(domainIDs)
            minvalues(i) = this%femdomain(domainIDs(i))%femdomainp%ymin()
        enddo
    else
        minvalues = zeros( size(this%femdomain) )
        do i=1,size(this%femdomain)
            minvalues(i) = this%femdomain(i)%femdomainp%ymin()
        enddo
    endif
    ret = minval(minvalues)

end function
! #####################################################

function zmax_Scene(this,domainIDs) result(ret)
    class(Scene_),intent(inout) :: this
    integer(int32),optional,intent(in) :: DomainIDs(:)
    real(real64),allocatable :: ret
    integer(int32) :: i
    real(real64),allocatable :: maxvalues(:)

    
    if(present(domainIDs) )then
        maxvalues = zeros(size(domainIDs) )
        do i=1,size(domainIDs)
            maxvalues(i) = this%femdomain(domainIDs(i))%femdomainp%zmax()
        enddo
    else
        maxvalues = zeros( size(this%femdomain) )
        do i=1,size(this%femdomain)
            maxvalues(i) = this%femdomain(i)%femdomainp%zmax()
        enddo
    endif
    ret = maxval(maxvalues)

end function
! #####################################################







! #####################################################
function zmin_Scene(this,domainIDs) result(ret)
    class(Scene_),intent(inout) :: this
    integer(int32),optional,intent(in) :: DomainIDs(:)
    real(real64),allocatable :: ret
    integer(int32) :: i
    real(real64),allocatable :: minvalues(:)

    
    if(present(domainIDs) )then
        minvalues = zeros(size(domainIDs) )
        do i=1,size(domainIDs)
            minvalues(i) = this%femdomain(domainIDs(i))%femdomainp%zmin()
        enddo
    else
        minvalues = zeros( size(this%femdomain) )
        do i=1,size(this%femdomain)
            minvalues(i) = this%femdomain(i)%femdomainp%zmin()
        enddo
    endif
    ret = minval(minvalues)

end function
! ###############################################
function full_function_Scene(this,func,params) result(ret)
    class(Scene_),intent(in) :: this
    interface 
        function func(x,params) result(ret)
            use iso_fortran_env
            implicit none
            real(real64),intent(in) :: x(:)
            real(real64),optional,intent(in) :: params(:)
            real(real64) :: ret
            
        end function
    end interface
    real(real64),optional,intent(in) :: params(:)
    real(real64),allocatable :: ret(:)
    integer(int32) :: domainID

    do DomainID=1,size(this%femdomain)
        if(DomainID==1)then
            ret = this%femdomain(DomainID)%femdomainp%full(func=func,params=params)
        else
            ret = ret // this%femdomain(DomainID)%femdomainp%full(func=func,params=params)
        endif
    enddo
end function
! #########################################################


! #########################################################
function femdomain_contacts_with_femdomain(obj1,obj2) result(ret)
    type(FEMDomain_),intent(in) :: obj1, obj2
    real(real64) :: xyz_ranges(3,2),center1(3),center2(3)
    logical :: ret

    ret = .false.

    ! Bounding Box Algorithm
    if(obj1%xmax()<obj2%xmin() ) return
    if(obj1%ymax()<obj2%ymin() ) return
    if(obj1%zmax()<obj2%zmin() ) return

    if(obj2%xmax()<obj1%xmin() ) return
    if(obj2%ymax()<obj1%ymin() ) return
    if(obj2%zmax()<obj1%zmin() ) return
    
    center1 = obj1%CenterPosition()
    center2 = obj2%CenterPosition()
    if(center1(1) < center2(1) )then
        xyz_ranges(1,1:2) = [obj2%xmin(),obj1%xmax()] ! x
    else
        xyz_ranges(1,1:2) = [obj1%xmin(),obj2%xmax()] ! x
    endif

    if(center1(2) < center2(2) )then
        xyz_ranges(2,1:2) = [obj2%ymin(),obj1%ymax()] ! y
    else
        xyz_ranges(2,1:2) = [obj1%ymin(),obj2%ymax()] ! y
    endif
    
    if(center1(3) < center2(3) )then
        xyz_ranges(3,1:2) = [obj2%zmin(),obj1%zmax()] ! z
    else
        xyz_ranges(3,1:2) = [obj1%zmin(),obj2%zmax()] ! z
    endif

    if (size(obj1%select( &
        x_min=xyz_ranges(1,1),&
        x_max=xyz_ranges(1,2),&
        y_min=xyz_ranges(2,1),&
        y_max=xyz_ranges(2,2),&
        z_min=xyz_ranges(3,1),&
        z_max=xyz_ranges(3,2) &
    ) )==0 ) return
    
    if (size(obj2%select( &
        x_min=xyz_ranges(1,1),&
        x_max=xyz_ranges(1,2),&
        y_min=xyz_ranges(2,1),&
        y_max=xyz_ranges(2,2),&
        z_min=xyz_ranges(3,1),&
        z_max=xyz_ranges(3,2) &
    ) )==0 ) return

    ret = .true.

end function
! #########################################################


end module

