module SeismicAnalysisClass
    use fem
    implicit none

    type::SeismicAnalysis_
        type(FEMDomain_),pointer :: femdomain
        real(real64),allocatable :: wave(:,:)
        real(real64) :: dt=1.0d0
        real(real64) :: t=0.0d0
        integer(int32) :: timestep=1
    contains
        procedure, public :: run => runSeismicAnalysis
        procedure, public :: CreateMatrixAndVector => CreateMatrixAndVectorSeismicAnalysis
    end type

contains

! ##############################################
subroutine runSeismicAnalysis(obj,dt,timestep,wave)
    class(SeismicAnalysis_),intent(inout) :: obj
    type(LinearSolver_) :: solver
    real(real64),optional,intent(in) :: dt
    integer(int32),optional,intent(in) :: timestep
    real(real64),optional,intent(in) :: wave(:,:)
    integer(int32) :: i

    obj%dt = input(default=0.0d0,option=dt)
    obj%timestep = input(default=1,option=timestep)
    if(present(wave) )then
        obj%wave = wave
    endif
    do i=1, obj%timestep
        obj%t = obj%t + dt
        call print("SeismicAnalysis >> "//str(obj%t-obj%dt)//"< t <"//str(obj%t)//" sec.")
        call obj%CreateMatrixAndVector(solver)
        call solver%solve(Solver="BiCGSTAB")
    enddo 
end subroutine
! ##############################################


! ##############################################
subroutine CreateMatrixAndVectorSeismicAnalysis(obj, solver)
    class(SeismicAnalysis_),intent(inout) :: obj
    type(LinearSolver_),intent(inout) :: solver
    integer(int32) :: i
    
    do i=1,obj%femdomain%ne()
        ! For each element
        ! Ax=b will be installed into solver
        
    enddo


end subroutine
! ##############################################



end module SeismicAnalysisClass