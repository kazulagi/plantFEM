module RouteOptimization
    use iso_fortran_env
    use GeometryClass
    implicit none
    type :: RouteOptimization_
        type(Point_),allocatable,private :: PointList(:)
        real(real64),allocatable,private :: Route(:,:)
        integer(int32), private :: NumOfPoint
        character*50 :: SolverName
    contains
        procedure, public :: init => initRouteOptimization
        procedure, public :: import => importRouteOptimization
        procedure, public :: setSolver => setSolverRouteOptimization
        procedure, public :: run => runRouteOptimization
        procedure, public :: export => exportRouteOptimization
    end type

contains

!##########################################
subroutine initRouteOptimization(obj,NumOfPoint,Dim)
    class(RouteOptimization_),intent(inout) :: obj
    integer(int32),optional,intent(in) :: NumOfPoint, Dim
    integer(int32) :: n,i,dimnum

    n = input(default=100,option=NumOfPoint)
    dimnum = input(default=2, option=Dim)
    allocate(obj%PointList(n) )
    allocate(obj%Route(n,dimnum) )
    obj%Route(:,:)=0.0d0
    do i=1, n
        call obj%PointList(i)%init(dim=Dim)
        obj%PointList(i)%coord(:) = 0.0d0
    enddo
    
end subroutine
!##########################################


!##########################################
subroutine importRouteOptimization(obj,Name)
    class(RouteOptimization_),intent(inout) :: obj
    character(*),intent(in) :: Name
    integer(int32) :: i,j,m,n,dim

    open(100,file=trim(Name))
    read(100,*) obj%NumOfPoint, dim
    call obj%init(NumOfPoint=obj%NumOfPoint,Dim=dim)
    do i=1,obj%NumOfPoint
        read(100,*) obj%PointList(i)%coord(:), obj%PointList(i)%name
        m=size(obj%PointList(i)%coord)
        do j=1,m
            obj%Route(i,j)=obj%PointList(i)%coord(j)
        enddo
    enddo
    close(100)

end subroutine
!##########################################

!##########################################
subroutine setSolverRouteOptimization(obj,SolverName)
    class(RouteOptimization_),intent(inout) :: obj
    character(*),intent(in) :: SolverName
    obj%SolverName =trim(SolverName)
end subroutine
!##########################################

!##########################################
subroutine runRouteOptimization(obj,SolverName, NumOfPoints)
    class(RouteOptimization_),intent(inout) :: obj
    character*200 :: command
    character(*),optional,intent(in) :: SolverName
    integer(int32),optional,intent(in) :: NumOfPoints
    integer(int32) :: i,n,old_id
    real(real64) :: x,y



    ! compile external solver
    if(present(SolverName) )then
        obj%SolverName=trim(SolverName)
    endif
    command = "mpic++ ./src/RouteOptimizationClass/"//trim(obj%SolverName)//".cpp -o "//trim(obj%SolverName)//".out"
    print *, command
    call system(trim(command))

    ! create input file for external solver
    n = input(default=size(obj%PointList) ,option=NumOfPoints)
    open(120,file="points.txt")
    write(120,*) n 
    do i=1, n
        write(120,*) obj%PointList(i)%coord(1:2)
    enddo
    close(120)
    command=""
    command="./"//trim(obj%SolverName)//".out"
    
    ! run!
    call system(trim(command))
end subroutine
!##########################################

!##########################################
subroutine exportRouteOptimization(obj,Repository)
    class(RouteOptimization_),intent(inout) :: obj
    character(*),intent(in) :: Repository
    integer(int32) :: i,n,old_id
    real(real64) :: x,y
    

    open(120,file="points.txt")
    read(120,*) n
    print *, "Number of points are ", n
    close(120)

    ! print initial route
    open(120, file=trim(Repository)//"/RouteOpt_InitialRoute.csv")
    write(120,*) 'latitude",longitude,Field Name'
    do i=1,n
        write(120,* ) obj%PointList(i)%coord(1),',', &
        obj%PointList(i)%coord(2),',', trim(obj%PointList(i)%Name)
    enddo
    close(120)

    ! import optimal route
    ! print initial route
    open(121, file=trim(Repository)//"/RouteOpt_OptimalRoute.csv")
    !open(121, file="RouteOpt_OptimalRoute.txt", status="replace")
    write(121,*) 'latitude,longitude,Field Name'
    open(120, file="solution.txt", status="old")
    do i=1,n
        read(120,* ) x,y, old_id
        write(121,* ) obj%PointList(old_id+1)%coord(1),',', &
        obj%PointList(old_id+1)%coord(2),',', trim(obj%PointList(old_id+1)%Name)
    enddo
    close(120)
    close(121)

end subroutine
!##########################################


end module