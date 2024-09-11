module ConstitutiveModelClass
   use, intrinsic :: iso_fortran_env
   use MathClass
   use StressClass

   implicit none
   type :: ConstitutiveModel_
      type(Stress_) :: Stress
      type(Strain_) :: Strain
      logical :: infinitesimal
   contains
      !procedure,public :: MCDP => MCDP_model
      !procedure,public :: CamClay => CamClay_model
      !procedure,public :: Jaumann => Jaumann_model
      !procedure,public :: Oldroyd => Oldroyd_model
      !procedure,public :: MCDP => MCDP_model
      !procedure,public :: CamClay => CamClay_model
      !procedure,public :: Jaumann => Jaumann_model
      !procedure,public :: Oldroyd => Oldroyd_model

   end type

   type :: ConstModel_
      real(real64), allocatable::sigma(:, :)
      real(real64), allocatable::S_IJ(:, :)
      real(real64), allocatable::M_IJ(:, :)
      real(real64), allocatable::tau(:, :)
      real(real64), allocatable::F_iJ(:, :)
      real(real64), allocatable::F_T(:, :)
      real(real64), allocatable::F_iJ_n(:, :)
      real(real64), allocatable::F_inv_iJ(:, :)
      real(real64), allocatable::F_T_inv_iJ(:, :)
      real(real64), allocatable::Fp_iJ(:, :)
      real(real64), allocatable::FT_Ij(:, :)
      real(real64), allocatable::b_ij(:, :)
      real(real64), allocatable::C_IJ(:, :)
      real(real64), allocatable::C_IJ_n(:, :)
      real(real64), allocatable::C_IJ_inv(:, :)
      real(real64), allocatable::E_IJ(:, :)
      real(real64), allocatable::Cp_IJ(:, :)
      real(real64), allocatable::Cp_IJ_inv(:, :)
      real(real64), allocatable::Cp_IJ_n(:, :)
      real(real64), allocatable::Bmat(:, :)

      real(real64), allocatable::StressDer(:, :, :, :)

      real(real64) :: detF
      real(real64) :: lamda
      real(real64) :: mu
      real(real64) :: K_mod
      real(real64) :: G_mod

      character*70::ModelType
      character*70::Config
   contains
      procedure, public :: getStress => HyperElasticStress
      procedure, public :: getStressDer => HyperElasticDer
   end type

contains
!#########################################################################
   subroutine HyperElasticStress(obj)
      Class(ConstModel_), intent(inout)::obj
      real(real64) :: a(3, 3), delta(3, 3)

      delta(:, :) = 0.0d0
      delta(1, 1) = 1.0d0
      delta(2, 2) = 1.0d0
      delta(3, 3) = 1.0d0
      if (obj%mu == 0) then
         stop "ERROR :: HyperElasticStress >> mu ==0"
      end if
      obj%K_mod = obj%lamda*(1.0d0 + obj%mu)/3.0d0/obj%mu
      obj%G_mod = obj%lamda*(1.0d0 - 2.0d0*obj%mu)/2.0d0/obj%mu

      obj%detF = det_mat(obj%F_iJ, 3)

      if (obj%detF == 0.0d0) then
         stop "ERROR :: HyperElasticStress >> detF=0.0d0"
      end if

      if (obj%ModelType /= obj%ModelType) then
         stop "ERROR :: HyperElasticStress >> Please set obj%ModelType"
      end if

      if (obj%ModelType == "StVenantKirchhoff") then
         if (.not. allocated(obj%sigma)) then
            allocate (obj%sigma(3, 3))
         end if
         if (.not. allocated(obj%tau)) then
            allocate (obj%tau(3, 3))
         end if
         a(:, :) = obj%b_ij(:, :) - delta(:, :)
         obj%tau(:, :) = 0.50d0*obj%lamda*(obj%b_ij(1, 1) + obj%b_ij(2, 2) + obj%b_ij(3, 3) - 3.0d0)*obj%b_ij(:, :) &
                         + obj%mu*matmul(obj%b_ij, a)
         obj%sigma(:, :) = 1.0d0/det_mat(obj%F_iJ, size(obj%F_iJ, 1))*obj%tau(:, :)

      elseif (obj%ModelType == "NeoHookean") then
         if (.not. allocated(obj%sigma)) then
            allocate (obj%sigma(3, 3))
         end if
         if (.not. allocated(obj%tau)) then
            allocate (obj%tau(3, 3))
         end if
         obj%tau(:, :) = obj%lamda*(log(obj%detF))*delta(:, :) + obj%mu*(obj%b_ij(:, :) - delta(:, :))
         obj%sigma(:, :) = 1.0d0/obj%detF*obj%tau(:, :)
      elseif (obj%ModelType == "ModifiedNeoHookean_Simo") then
         if (.not. allocated(obj%sigma)) then
            allocate (obj%sigma(3, 3))
         end if
         if (.not. allocated(obj%tau)) then
            allocate (obj%tau(3, 3))
         end if
         obj%tau(:, :) = 0.50d0*obj%lamda*((obj%detF)*(obj%detF) - 1.0d0)*delta(:, :) &
                         + obj%mu*(obj%b_ij(:, :) - delta(:, :))
         obj%sigma(:, :) = 1.0d0/obj%detF*obj%tau

      elseif (obj%ModelType == "ModifiedNeoHookean_Vlad") then
         if (.not. allocated(obj%sigma)) then
            allocate (obj%sigma(3, 3))
         end if
         if (.not. allocated(obj%tau)) then
            allocate (obj%tau(3, 3))
         end if

         obj%tau(:, :) = 0.50d0*obj%lamda*((obj%detF)*(obj%detF) - 1.0d0)*delta(:, :) &
                         + obj%mu*(obj%b_ij(:, :) - delta(:, :))
         obj%sigma(:, :) = 1.0d0/obj%detF*obj%tau
      else
         print *, "Sorry, the model ", obj%ModelType, " is not implemented yet."
         stop
      end if
   end subroutine
!#########################################################################

!#########################################################################
   subroutine HyperElasticDer(obj, DerType)
      Class(ConstModel_), intent(inout)::obj
      character*70, intent(in)::DerType
      real(real64) :: a(3, 3), a1(3, 3), delta(3, 3)
      integer(int32) :: i, j, k, l

      delta(:, :) = 0.0d0
      delta(1, 1) = 1.0d0
      delta(2, 2) = 1.0d0
      delta(3, 3) = 1.0d0
      if (obj%mu == 0) then
         stop "ERROR :: HyperElasticStress >> mu ==0"
      end if

      obj%K_mod = obj%lamda*(1.0d0 + obj%mu)/3.0d0/obj%mu
      obj%G_mod = obj%lamda*(1.0d0 - 2.0d0*obj%mu)/2.0d0/obj%mu

      if (obj%detF /= obj%detF) then
         obj%detF = det_mat(obj%F_iJ, 3)
      end if
      if (obj%detF == 0.0d0) then
         stop "ERROR :: HyperElasticStress >> detF=0.0d0"
      end if

      if (obj%ModelType /= obj%ModelType) then
         stop "ERROR :: HyperElasticStress >> Please set obj%ModelType"
      end if

      if (obj%ModelType == "StVenantKirchhoff") then
         if (.not. allocated(obj%StressDer)) then
            allocate (obj%StressDer(3, 3, 3, 3))
         end if
         if (DerType == "F" .or. DerType == "F_iJ") then
            obj%StressDer(:, :, :, :) = 0.0d0
            do i = 1, 3
               do j = 1, 3
                  do k = 1, 3
                     do l = 1, 3

                        a(:, :) = matmul(obj%b_ij, obj%F_ij)
                        a1(:, :) = matmul(obj%b_ij, trans2(obj%F_ij))
                        obj%StressDer(i, j, k, l) = obj%StressDer(i, j, k, l) &
                                           - 0.50d0/obj%detF*obj%F_inv_ij(l, k)*(trace(obj%b_ij) - 3.0d0)*obj%b_ij(i, j)*obj%lamda &
                                                    + 0.50d0/obj%detF*delta(k, l)*obj%b_ij(i, j)*obj%lamda &
                                                    + 0.50d0/obj%detF*(trace(obj%b_ij) - 3.0d0)*(delta(i, k)*obj%F_ij(j, l) + &
                                                                                             obj%F_ij(i, l)*delta(j, k))*obj%lamda &
                                                    + obj%mu*(-delta(i, k)*obj%F_ij(j, l) + obj%F_ij(i, l)*obj%b_ij(k, j)) &
                                                    + obj%mu*(obj%F_ij(j, l)*obj%b_ij(i, k) + a(i, l)*delta(j, k))
                     end do
                  end do
               end do
            end do
         elseif (DerType == "c_current" .or. DerType == "cc") then
            obj%StressDer(:, :, :, :) = 0.0d0

            do i = 1, 3
               do j = 1, 3
                  do k = 1, 3
                     do l = 1, 3
                        obj%StressDer(i, j, k, l) = obj%StressDer(i, j, k, l) &
                                                    + obj%lamda*obj%b_ij(i, j)*obj%b_ij(k, l) &
                                                    + obj%mu*obj%b_ij(i, k)*obj%b_ij(j, l) &
                                                    + obj%mu*obj%b_ij(i, l)*obj%b_ij(j, k)
                     end do
                  end do
               end do
            end do
         else
            stop "ERROR :: HyperElasticStress >> no such der"
         end if
      elseif (obj%ModelType == "NeoHookean") then
         if (.not. allocated(obj%StressDer)) then
            allocate (obj%StressDer(3, 3, 3, 3))
         end if
         if (DerType == "F" .or. DerType == "F_iJ") then
            obj%StressDer(:, :, :, :) = 0.0d0
            do i = 1, 3
               do j = 1, 3
                  do k = 1, 3
                     do l = 1, 3
                        obj%StressDer(i, j, k, l) = obj%StressDer(i, j, k, l) &
                                                    - obj%lamda/obj%detF*obj%F_inv_iJ(l, k)*log(obj%detF)*delta(i, j) &
                                                    + obj%lamda/obj%detF*obj%F_inv_iJ(l, k)*delta(i, j) &
                                                    - obj%mu/obj%detF*obj%F_inv_iJ(l, k)*(obj%b_ij(i, j) - delta(i, j)) &
                                                    + obj%mu/obj%detF*(delta(i, k)*obj%F_ij(l, j) + delta(j, l)*obj%F_ij(i, k))

                     end do
                  end do
               end do
            end do

         elseif (DerType == "c_current" .or. DerType == "cc") then
            obj%StressDer(:, :, :, :) = 0.0d0

            do i = 1, 3
               do j = 1, 3
                  do k = 1, 3
                     do l = 1, 3
                        obj%StressDer(i, j, k, l) = obj%StressDer(i, j, k, l) &
                                                    + obj%lamda*delta(i, j)*delta(k, l) &
                                                    + (obj%mu - obj%lamda*(log(obj%detF)))*(delta(i, k)*delta(j, l)) &
                                                    + (obj%mu - obj%lamda*(log(obj%detF)))*(delta(i, l)*delta(k, j))
                     end do
                  end do
               end do
            end do
         else
            stop "ERROR :: HyperElasticStress >> no such der"
         end if
      elseif (obj%ModelType == "ModifiedNeoHookean_Simo") then
         if (.not. allocated(obj%StressDer)) then
            allocate (obj%StressDer(3, 3, 3, 3))
         end if
         if (DerType == "F" .or. DerType == "F_iJ") then
            obj%StressDer(:, :, :, :) = 0.0d0
            do i = 1, 3
               do j = 1, 3
                  do k = 1, 3
                     do l = 1, 3
                        obj%StressDer(i, j, k, l) = obj%StressDer(i, j, k, l) &
                                                    - obj%lamda/2.0d0*(obj%detF + 1.0d0/obj%detF)*obj%F_inv_iJ(l, k)*delta(i, j) &
                                                    - obj%mu/obj%detF*obj%F_inv_iJ(l, k)*(obj%b_ij(i, j) - delta(i, j)) &
                                                    + obj%mu/obj%detF*(delta(i, k)*obj%F_ij(l, j) + delta(j, l)*obj%F_ij(i, k))
                     end do
                  end do
               end do
            end do
         else
            stop "ERROR :: HyperElasticStress >> no such der"
         end if
      elseif (obj%ModelType == "ModifiedNeoHookean_Vlad") then
         if (.not. allocated(obj%StressDer)) then
            allocate (obj%StressDer(3, 3, 3, 3))
         end if
         if (DerType == "F" .or. DerType == "F_iJ") then
            obj%StressDer(:, :, :, :) = 0.0d0
            do i = 1, 3
               do j = 1, 3
                  do k = 1, 3
                     do l = 1, 3
                        obj%StressDer(i, j, k, l) = obj%StressDer(i, j, k, l) &
                                                    - obj%lamda/2.0d0*(obj%detF + 1.0d0/obj%detF)*obj%F_inv_iJ(l, k)*delta(i, j) &
                                                    - obj%mu/obj%detF*obj%F_inv_iJ(l, k)*(obj%b_ij(i, j) - delta(i, j)) &
                                                    + obj%mu/obj%detF*(delta(i, k)*obj%F_ij(l, j) + delta(j, l)*obj%F_ij(i, k))
                     end do
                  end do
               end do
            end do
         else
            stop "ERROR :: HyperElasticStress >> no such der"
         end if
      else
         print *, "Sorry, the model ", obj%ModelType, " is not implemented yet."
         stop
      end if
   end subroutine
!#########################################################################

end module
