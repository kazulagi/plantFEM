module ContactDiffusionClass

    use, intrinsic :: iso_fortran_env
	use MathClass
    use FEMIfaceClass
	use FEMDomainClass
	use FiniteDeformationClass
    
    implicit none

    type :: ContactDiffusion_
		! Modern 
		type(FEMDomainp_),allocatable :: FEMDomains(:)
		type(LinearSolver_) :: solver
		integer(int32),allocatable :: contactlist(:,:)
		real(real64),allocatable :: YoungModulus(:)
		real(real64),allocatable :: PoissonRatio(:)
		real(real64),allocatable :: Density(:)

		real(real64),allocatable :: YoungModulusList(:,:)
		real(real64),allocatable :: PoissonRatioList(:,:)
		real(real64),allocatable :: DensityList(:,:)

		logical :: initialized = .false.

		real(real64) :: gravity(1:3) =[0.0d0, 0.0d0, -9.810d0]
		
		real(real64) :: penalty = 100000.0d0
    contains
        procedure :: Init			=> InitializeContactDiffusion
        procedure :: setup 			=> runContactDiffusion
        procedure :: fix 			=> fixContactDiffusion
        procedure :: solve 			=> solveContactDiffusion
    end type
contains



! ###############################################################
subroutine InitializeContactDiffusion(obj)
    class(ContactDiffusion_),intent(inout) :: obj

end subroutine
! ###############################################################

! ###############################################################
subroutine runContactDiffusion(obj)
    class(ContactDiffusion_),intent(inout) :: obj

end subroutine
! ###############################################################

! ###############################################################
subroutine fixContactDiffusion(obj)
    class(ContactDiffusion_),intent(inout) :: obj

end subroutine
! ###############################################################

! ###############################################################
subroutine solveContactDiffusion(obj)
    class(ContactDiffusion_),intent(inout) :: obj

end subroutine
! ###############################################################

end module ContactDiffusionClass