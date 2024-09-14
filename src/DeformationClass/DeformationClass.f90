module DeformationClass
   use iso_fortran_env
   use StrainClass
   use StressClass
   use StiffnessMatrixClass
   implicit none

   type :: Deformation_
      type(FEMDomain_)       :: FEMDomain
      type(Stress_), allocatable :: Stress(:, :) ! for Elements, for Gauss-points
      type(Strain_), allocatable :: Strain(:, :) ! for Elements, for Gauss-points
      type(StiffnessMatrix_)    :: StifMat
      real(real64)     :: tol

   contains
      procedure :: init => initDeformation
      procedure :: update => updateDeformation
      procedure :: solve => solveDeformation
      procedure :: converge => convergeDeformation
   end type

contains

!#############################################
   subroutine initDeformation(obj)
      class(Deformation_), intent(inout) :: obj

   end subroutine
!#############################################

!#############################################
   subroutine updateDeformation(obj, itrmax)
      class(Deformation_), intent(inout) :: obj
      character(40):: StainTheory, ConstitutiveModel, TimeIntegral
      integer :: i, j, k, NumOfElement, NumOfGaussPoint, itr

      StainTheory = obj%FEMDomain%Category1
      ConstitutiveModel = obj%FEMDomain%Category2
      TimeIntegral = obj%FEMDomain%Category3

      do itr = 1, itrmax
         ! update Ax=b
         do i = 1, NumOfElement
            do j = 1, NumOfGaussPoint

               ! Which is better, (GetStress + get EBE) or (getEbE)?
               call obj%Stress%update(FEMDomain=obj%FEMDomain, &
                                      Stress=obj%Stress, Strain=obj%Strain, &
                                      StrainTheory=StainTheory, &
                                      ConstitutiveModel=ConstitutiveModel,
               TimeIntegral = TimeIntegral) ! get Stress
               call obj%StifMat%update(FEMDomain=obj%FEMDomain, &
                                       Stress=obj%Stress, Strain=obj%Strain, &
                                       StrainTheory=StainTheory, &
                                       ConstitutiveModel=ConstitutiveModel,
               TimeIntegral = TimeIntegral) ! get EbE matrices
            end do
         end do

         ! solve x=A^(-1)b and update kinetic field
         call obj%solve(StrainTheory=StainTheory, TimeIntegral=TimeIntegral)

         ! check convergence
         if (obj%Converge() .eqv. .true.) then
            exit
         end if
      end do

   end subroutine
!#############################################

!#############################################
   subroutine solveDeformation(obj)
      class(Deformation_), intent(inout) :: obj

      ! solve Ax=B and update kinetic field

   end subroutine
!#############################################

!#############################################
   function convergeDeformation(obj) result(yes_or_no)
      class(Deformation_), intent(inout) :: obj
      logical :: yes_or_no

   end function
!#############################################

end module
