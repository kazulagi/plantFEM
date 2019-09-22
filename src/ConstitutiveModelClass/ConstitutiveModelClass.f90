module ConstitutiveModelClass
    use MathClass

    implicit none

    type :: ConstModel_
        real(8),allocatable::sigma(:,:)
        real(8),allocatable::S_IJ(:,:)
        real(8),allocatable::M_IJ(:,:)
        real(8),allocatable::tau(:,:)
        real(8),allocatable::F_iJ(:,:)
        real(8),allocatable::F_T(:,:)
        real(8),allocatable::F_iJ_n(:,:)
        real(8),allocatable::F_inv_iJ(:,:)
        real(8),allocatable::F_T_inv_iJ(:,:)
        real(8),allocatable::Fp_iJ(:,:)
        real(8),allocatable::FT_Ij(:,:)
        real(8),allocatable::b_ij(:,:)
        real(8),allocatable::C_IJ(:,:)
        real(8),allocatable::C_IJ_n(:,:)
        real(8),allocatable::C_IJ_inv(:,:)
        real(8),allocatable::E_IJ(:,:)
        real(8),allocatable::Cp_IJ(:,:)
        real(8),allocatable::Cp_IJ_inv(:,:)
        real(8),allocatable::Cp_IJ_n(:,:)
        real(8),allocatable::Bmat(:,:)


        real(8),allocatable::StressDer(:,:,:,:)

        real(8) :: detF
        real(8) :: lamda
        real(8) :: mu
        real(8) :: K_mod
        real(8) :: G_mod
        
        
        character*70::ModelType
        character*70::Config    
    end type
contains
!#########################################################################
subroutine HyperElasticStress(obj)
    type(ConstModel_),intent(inout)::obj
    real(8) :: a(3,3),delta(3,3)

    delta(:,:)=0.0d0
    delta(1,1)=1.0d0
    delta(2,2)=1.0d0
    delta(3,3)=1.0d0
    if(obj%mu==0)then
        stop "ERROR :: HyperElasticStress >> mu ==0"
    endif
    obj%K_mod=obj%lamda*(1.0d0 +     obj%mu)/3.0d0/obj%mu
    obj%G_mod=obj%lamda*(1.0d0-2.0d0*obj%mu)/2.0d0/obj%mu

    
    obj%detF=det_mat(obj%F_iJ,3)
    
    if(obj%detF==0.0d0)then
        stop "ERROR :: HyperElasticStress >> detF=0.0d0"
    endif

    if(obj%ModelType/=obj%ModelType)then
        stop "ERROR :: HyperElasticStress >> Please set obj%ModelType"
    endif

    if(trim(obj%ModelType)=="StVenantKirchhoff" )then
        if(.not.allocated(obj%sigma) )then
            allocate(obj%sigma(3,3) )
        endif
        if(.not.allocated(obj%tau) )then
            allocate(obj%tau(3,3) )
        endif
        a(:,:)=obj%b_ij(:,:)-delta(:,:)
        obj%tau(:,:)=0.50d0*obj%lamda*(obj%b_ij(1,1)+obj%b_ij(2,2)+obj%b_ij(3,3) -3.0d0 )*obj%b_ij(:,:)&
            + obj%mu*matmul(obj%b_ij, a )
        obj%sigma(:,:)=1.0d0/det_mat(obj%F_iJ, size(obj%F_iJ,1))*obj%tau(:,:)
    elseif(trim(obj%ModelType)=="NeoHookean" )then
        if(.not.allocated(obj%sigma) )then
            allocate(obj%sigma(3,3) )
        endif
        if(.not.allocated(obj%tau) )then
            allocate(obj%tau(3,3) )
        endif
        obj%tau(:,:)=obj%lamda*(log(obj%detF) )*delta(:,:) + obj%mu*(obj%b_ij(:,:)-delta(:,:) )
        obj%sigma(:,:)=1.0d0/obj%detF*obj%tau(:,:)
    elseif(trim(obj%ModelType)=="ModifiedNeoHookean_Simo" )then
        if(.not.allocated(obj%sigma) )then
            allocate(obj%sigma(3,3) )
        endif
        if(.not.allocated(obj%tau) )then
            allocate(obj%tau(3,3) )
        endif
        obj%tau(:,:)=0.50d0*obj%lamda*((obj%detF)*(obj%detF)-1.0d0 )*delta(:,:) &
            + obj%mu*(obj%b_ij(:,:)-delta(:,:) )
        obj%sigma(:,:)=1.0d0/obj%detF*obj%tau

        
    elseif(trim(obj%ModelType)=="ModifiedNeoHookean_Vlad" )then
        if(.not.allocated(obj%sigma) )then
            allocate(obj%sigma(3,3) )
        endif
        if(.not.allocated(obj%tau) )then
            allocate(obj%tau(3,3) )
        endif
        
        obj%tau(:,:)=0.50d0*obj%lamda*((obj%detF)*(obj%detF)-1.0d0 )*delta(:,:) &
            + obj%mu*(obj%b_ij(:,:)-delta(:,:) )
        obj%sigma(:,:)=1.0d0/obj%detF*obj%tau
    else
        print *, "Sorry, the model ",trim(obj%ModelType)," is not implemented yet."
        stop
    endif
end subroutine
!#########################################################################


!#########################################################################
subroutine HyperElasticDer(obj,DerType)
    type(ConstModel_),intent(inout)::obj
    character*70,intent(in)::DerType
    real(8) :: a(3,3),a1(3,3),delta(3,3)
    integer :: i,j,k,l

    delta(:,:)=0.0d0
    delta(1,1)=1.0d0
    delta(2,2)=1.0d0
    delta(3,3)=1.0d0
    if(obj%mu==0)then
        stop "ERROR :: HyperElasticStress >> mu ==0"
    endif
    
    obj%K_mod=obj%lamda*(1.0d0 +     obj%mu)/3.0d0/obj%mu
    obj%G_mod=obj%lamda*(1.0d0-2.0d0*obj%mu)/2.0d0/obj%mu

    if(obj%detF/=obj%detF)then
        obj%detF=det_mat(obj%F_iJ,3)
    endif
    if(obj%detF==0.0d0)then
        stop "ERROR :: HyperElasticStress >> detF=0.0d0"
    endif

    if(obj%ModelType/=obj%ModelType)then
        stop "ERROR :: HyperElasticStress >> Please set obj%ModelType"
    endif

    if(trim(obj%ModelType)=="StVenantKirchhoff" )then
        if(.not.allocated(obj%StressDer) )then
            allocate(obj%StressDer(3,3,3,3) )
        endif
        if(trim(DerType)=="F" .or. trim(DerType)=="F_iJ"  )then
            obj%StressDer(:,:,:,:)=0.0d0
            do i=1,3
                do j=1,3
                    do k=1,3
                        do l=1,3

                            a(:,:)=matmul(obj%b_ij,obj%F_ij)
                            a1(:,:)=matmul(obj%b_ij,trans2(obj%F_ij))
                            obj%StressDer(i,j,k,l)=obj%StressDer(i,j,k,l)&
                            -0.50d0/obj%detF*obj%F_inv_ij(l,k)*(trace(obj%b_ij)-3.0d0 )*obj%b_ij(i,j)*obj%lamda&
                            +0.50d0/obj%detF*delta(k,l)*obj%b_ij(i,j)*obj%lamda&
                            +0.50d0/obj%detF*(trace(obj%b_ij) - 3.0d0 )*(delta(i,k)*obj%F_ij(j,l)+&
                            obj%F_ij(i,l)*delta(j,k) )*obj%lamda&
                            +obj%mu*(-delta(i,k)*obj%F_ij(j,l)+obj%F_ij(i,l)*obj%b_ij(k,j) )&
                            +obj%mu*( obj%F_ij(j,l)*obj%b_ij(i,k) + a(i,l)*delta(j,k)  )
                        enddo
                    enddo
                enddo
            enddo
        elseif(trim(DerType)=="c_current" .or. trim(DerType)=="cc"  )then
            obj%StressDer(:,:,:,:)=0.0d0

            do i=1,3
                do j=1,3
                    do k=1,3
                        do l=1,3
                            obj%StressDer(i,j,k,l)=obj%StressDer(i,j,k,l)&
                           + obj%lamda*obj%b_ij(i,j)*obj%b_ij(k,l)&
                           + obj%mu*obj%b_ij(i,k)*obj%b_ij(j,l)&
                           + obj%mu*obj%b_ij(i,l)*obj%b_ij(j,k)    
                        enddo
                    enddo
                enddo
            enddo
        else
            stop "ERROR :: HyperElasticStress >> no such der"
        endif
    elseif(trim(obj%ModelType)=="NeoHookean" )then
        if(.not.allocated(obj%StressDer) )then
            allocate(obj%StressDer(3,3,3,3) )
        endif
        if(trim(DerType)=="F" .or. trim(DerType)=="F_iJ"  )then
            obj%StressDer(:,:,:,:)=0.0d0
            do i=1,3
                do j=1,3
                    do k=1,3
                        do l=1,3
                            obj%StressDer(i,j,k,l)=obj%StressDer(i,j,k,l)&
                            -obj%lamda/obj%detF*obj%F_inv_iJ(l,k)*log(obj%detF)*delta(i,j)&
                            +obj%lamda/obj%detF*obj%F_inv_iJ(l,k)*delta(i,j)&
                            -obj%mu/obj%detF*obj%F_inv_iJ(l,k)*(obj%b_ij(i,j)-delta(i,j) )&
                            +obj%mu/obj%detF*(delta(i,k)*obj%F_ij(l,j) + delta(j,l)*obj%F_ij(i,k))

                        enddo
                    enddo
                enddo
            enddo
        
        elseif(trim(DerType)=="c_current" .or. trim(DerType)=="cc"  )then
            obj%StressDer(:,:,:,:)=0.0d0

            do i=1,3
                do j=1,3
                    do k=1,3
                        do l=1,3
                            obj%StressDer(i,j,k,l)=obj%StressDer(i,j,k,l)&
                           + obj%lamda*delta(i,j)*delta(k,l)&
                           + (obj%mu - obj%lamda*(log(obj%detF) )) * ( delta(i,k)*delta(j,l))&
                           + (obj%mu - obj%lamda*(log(obj%detF) )) * ( delta(i,l)*delta(k,j))    
                        enddo
                    enddo
                enddo
            enddo
        else
            stop "ERROR :: HyperElasticStress >> no such der"
        endif
    elseif(trim(obj%ModelType)=="ModifiedNeoHookean_Simo" )then
        if(.not.allocated(obj%StressDer) )then
            allocate(obj%StressDer(3,3,3,3) )
        endif
        if(trim(DerType)=="F" .or. trim(DerType)=="F_iJ"  )then
            obj%StressDer(:,:,:,:)=0.0d0
            do i=1,3
                do j=1,3
                    do k=1,3
                        do l=1,3
                            obj%StressDer(i,j,k,l)=obj%StressDer(i,j,k,l)&
                            -obj%lamda/2.0d0*( obj%detF + 1.0d0/obj%detF  )*obj%F_inv_iJ(l,k)*delta(i,j)&
                            -obj%mu/obj%detF*obj%F_inv_iJ(l,k)*(obj%b_ij(i,j)-delta(i,j) )&
                            +obj%mu/obj%detF*(delta(i,k)*obj%F_ij(l,j) + delta(j,l)*obj%F_ij(i,k))
                        enddo
                    enddo
                enddo
            enddo
        else
            stop "ERROR :: HyperElasticStress >> no such der"
        endif
    elseif(trim(obj%ModelType)=="ModifiedNeoHookean_Vlad" )then
        if(.not.allocated(obj%StressDer) )then
            allocate(obj%StressDer(3,3,3,3) )
        endif
        if(trim(DerType)=="F" .or. DerType=="F_iJ"  )then
            obj%StressDer(:,:,:,:)=0.0d0
            do i=1,3
                do j=1,3
                    do k=1,3
                        do l=1,3
                            obj%StressDer(i,j,k,l)=obj%StressDer(i,j,k,l)&
                            -obj%lamda/2.0d0*( obj%detF + 1.0d0/obj%detF  )*obj%F_inv_iJ(l,k)*delta(i,j)&
                            -obj%mu/obj%detF*obj%F_inv_iJ(l,k)*(obj%b_ij(i,j)-delta(i,j) )&
                            +obj%mu/obj%detF*(delta(i,k)*obj%F_ij(l,j) + delta(j,l)*obj%F_ij(i,k))
                        enddo
                    enddo
                enddo
            enddo
        else
            stop "ERROR :: HyperElasticStress >> no such der"
        endif
    else
        print *, "Sorry, the model ",trim(obj%ModelType)," is not implemented yet."
        stop
    endif
end subroutine
!#########################################################################

end module