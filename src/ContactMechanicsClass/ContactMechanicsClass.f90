module ContactMechanicsClass
    use, intrinsic :: iso_fortran_env
	use MathClass
!	use MPIClass
    use FEMIfaceClass
	use FEMDomainClass
	use FiniteDeformationClass
    
    implicit none

    type :: ContactMechanics_
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
		real(real64),allocatable :: Displacement(:)
		real(real64),allocatable :: TractionForce(:,:)
		

		logical :: initialized = .false.

		real(real64) :: gravity(1:3) =[0.0d0, 0.0d0, -9.810d0]
		
		real(real64) :: penalty = 100000.0d0

		! >>>>>>>>>>>> Regacy >>>>>>>>>>>>>>>>>
		! >>>>>>>>>>>> Regacy >>>>>>>>>>>>>>>>>
		! >>>>>>>>>>>> Regacy >>>>>>>>>>>>>>>>>
		! >>>>>>>>>>>> Regacy >>>>>>>>>>>>>>>>>
		! 
		type(FEMDomain_),pointer::FEMDomain1
		type(FEMDomain_),pointer::FEMDomain2
	
		type(FEMIface_),pointer::FEMIface
		! common fields
		real(real64),allocatable		:: NTSGap(:,:)
		real(real64),allocatable		:: NTSGzi(:,:)
		real(real64)		  			:: penaltypara=dble(1.0e+5)
		real(real64)		  			:: FrictionalCoefficient=0.30d0
		real(real64)		  			:: Cohesion=0.0d0
		real(real64)		  			:: Tolerance=dble(1.0e-10)

		! for weak coupling contact analysis
		real(real64),allocatable    :: Domain1Force(:,:)
		real(real64),allocatable    :: Domain2Force(:,:)


		! for strong coupling contact analysys
        real(real64),allocatable    ::KcontactEBE(:,:,:)
        real(real64),allocatable    ::KcontactGlo(:,:)
        real(real64),allocatable    ::FcontactEBE(:,:)
        real(real64),allocatable    ::FcontactGlo(:)
        real(real64),allocatable    ::DispVecEBE(:,:)
        real(real64),allocatable    ::DispVecGlo(:)
        real(real64),allocatable    ::NTSvariables(:,:)
        real(real64),allocatable    ::ContactMatPara(:,:)
		real(real64),allocatable    ::GloNodCoord(:,:)
		
		! boundary conditions for lodging simulator 2.5
		integer(int32),allocatable    ::u_nod_x(:)
		integer(int32),allocatable    ::u_nod_y(:)
		integer(int32),allocatable    ::u_nod_z(:)
		real(real64),allocatable    ::du_nod_dis_x(:)
		real(real64),allocatable    ::du_nod_dis_y(:)
		real(real64),allocatable    ::du_nod_dis_z(:)
		real(real64),allocatable    ::u_nod_dis_x(:)
		real(real64),allocatable    ::u_nod_dis_y(:)
		real(real64),allocatable    ::u_nod_dis_z(:)
		real(real64),allocatable		:: duvec(:)
		real(real64),allocatable		::  uvec(:)
		real(real64),allocatable		:: dfvec(:)
		real(real64),allocatable		::  fvec(:)

        integer(int32),allocatable    ::NTSMaterial(:)
		integer(int32),allocatable    ::StickOrSlip(:)
	
		integer(int32) :: step=0
		integer(int32) :: itr_contact=0
		integer(int32) :: itr=0
		integer(int32) :: BiCG_ItrMax=10000
		integer(int32) :: NR_ItrMax=100
		integer(int32) :: control=1 ! 1:displacement-control, 2: traction-control
		integer(int32) :: TimeStep=100

		! from lodging-simulatiro 2.5

		integer(int32),allocatable    ::nts_elem_nod(:,:)
		integer(int32),allocatable    ::old_nts_elem_nod(:,:)
		integer(int32),allocatable    ::surface_nod(:)
		integer(int32),allocatable    ::sur_nod_inf(:,:)
		real(real64),allocatable    ::nod_coord(:,:)
		real(real64),allocatable    ::old_nod_coord(:,:)
		real(real64),allocatable    ::elem_nod(:,:)
		integer(int32),allocatable  :: nts_mat(:)
		integer(int32),allocatable  :: sur_inf_mat(:,:)
		integer(int32),allocatable  :: contact_mat(:,:)
		real(real64),allocatable    ::contact_mat_para(:,:)
		integer(int32),allocatable  :: active_nts(:)

		real(real64),allocatable    ::k_contact(:,:)
		real(real64),allocatable    ::fvec_contact(:)
		real(real64),allocatable    ::nts_amo(:,:)
		integer(int32),allocatable  :: stick_slip(:)
		integer(int32),allocatable  :: old_stick_slip(:)
		real(real64),allocatable    ::old_nts_amo(:,:)
		real(real64),allocatable    ::kmat(:,:),gvec(:),rvec(:)
		real(real64),allocatable    ::K_total(:,:),initial_duvec(:),dduvec(:),dduvec_nr(:)


		
	contains
		! modern
		procedure :: Init			=> InitializeContactMechanics
		procedure :: setup 			=> runCM
		procedure :: run 			=> runCM
		procedure :: solve 			=> solveCM
		procedure :: updateMesh     => updateMeshContactMechanics
		procedure :: fix     		=> fixContactMechanics
		procedure :: setDensity     => setDensity
		procedure :: setYoungModulus     => setYoungModulus
		procedure :: setPoissonRatio     => setPoissonRatio
		procedure :: properties => propertiesCM
		procedure :: property => propertiesCM
		procedure :: showProperty   => showPropertyCM
		procedure :: remove         => removeContactMechanics


	
		! regacy
		procedure :: Update			=> UpdateContactConfiguration
		procedure :: Import         => ImportContactMechanics 
		procedure :: deploy			=> deployContactMechanics
		procedure :: ContactSearch  => ContactSearch 
        procedure :: getKcmat       => getKcmat
        procedure :: getKcmatStick  => getKcmatStick
		procedure :: getKcmatStickSlip 		=> getKcmatStickSlip 
		procedure :: setPenaltyParameter 	=> setPenaltyParaCM 
		procedure :: updateContactStress 	=> updateContactStressCM
		procedure :: updateTimestep => updateTimestepContact
		procedure :: getGap			=> getGapCM
		procedure :: getForce		=> getForceCM
		procedure :: exportForceAsTraction 	=> exportForceAsTractionCM
		procedure :: getDispBound => getDispBoundCM
		procedure :: getTracBound => getTracBoundCM


		! >>> regacy subroutines for lodging-simulator 2.5
		procedure :: ls_add_du => ls_add_duCM
		procedure :: ls_nts_generate => ls_nts_generateCM
		procedure :: ls_nts_material => ls_nts_materialCM
		procedure :: ls_get_stabilized_nts => ls_get_stabilized_ntsCM
		procedure :: ls_check_active => ls_check_active_CM
    end type

contains



! #####################################################
subroutine InitializeContactMechanics(obj, femdomains, femdomainsp, contactlist,femdomain1, femdomain2,&
	AllYoungModulus,AllPoissonRatio,AllDensity)
	class(ContactMechanics_),intent(inout)  :: obj
	type(FEMDomain_),target,optional,intent(in) :: femdomains(:)
	type(FEMDomainp_),target,optional,intent(in) :: femdomainsp(:)
	type(FEMDomain_),target,optional,intent(in) :: femdomain1, femdomain2
	integer(int32),optional,intent(in) :: ContactList(:,:)

	real(real64) :: DefaultYoungModulus=1000.0d0
	real(real64) :: DefaultPoissonRatio=0.30d0
	real(real64) :: DefaultDensity=0.0d0
	
	real(real64),optional,intent(in) :: AllYoungModulus
	real(real64),optional,intent(in) :: AllPoissonRatio
	real(real64),optional,intent(in) :: AllDensity

	integer(int32) :: node_num_1
	integer(int32) :: node_num_2

	integer(int32) :: numDomain,i
	integer(int32),allocatable :: NumberOfNode(:)

	! modern

	if(present(AllYoungModulus) )then
		DefaultYoungModulus = AllYoungModulus 
	endif

	if(present(AllPoissonRatio) )then
		DefaultPoissonRatio = AllPoissonRatio 
	endif
	
	if(present(AllDensity) )then
		DefaultDensity = AllDensity 
	endif
	
	if(present(femdomains) .and. present(contactList) )then
		numDomain = size(femdomains)
		if(numDomain==0)then
			print *, "[Caution] :: InitializeContactMechanics >> No domain was found in femdomains=***"
			print *, "             as well as contactlist=***"
			return
		endif

		if(allocated(obj%femdomains) )then
			deallocate(obj%femdomains)
		endif

		allocate(obj%femdomains(numDomain) )
		allocate(obj%YoungModulus(numDomain) )
		allocate(obj%PoissonRatio(numDomain) )
		allocate(obj%Density(numDomain) )
		obj%YoungModulus(:) = DefaultYoungModulus
		obj%PoissonRatio(:) = DefaultPoissonRatio
		obj%Density(:) = DefaultDensity
		


		! receive domains as pointers
		do i=1,numDomain
			obj%femdomains(i)%femdomainp => femdomains(i)
		enddo

		obj%ContactList = contactList


		! initialize solver
		allocate(NumberOfNode(numDomain))
		do i=1,numDomain
			NumberOfNode(i) = obj%femdomains(i)%femdomainp%nn()
		enddo
		call obj%solver%init(NumberOfNode=NumberOfNode, DOF = obj%femdomains(1)%femdomainp%nd() )

		obj%initialized = .true.
		return
	elseif(present(femdomainsp) .and. present(contactList) )then
		numDomain = size(femdomainsp)
		if(numDomain==0)then
			print *, "[Caution] :: InitializeContactMechanics >> No domain was found in femdomains=***"
			print *, "             as well as contactlist=***"
			return
		endif

		if(allocated(obj%femdomains) )then
			deallocate(obj%femdomains)
		endif

		allocate(obj%femdomains(numDomain) )
		allocate(obj%YoungModulus(numDomain) )
		allocate(obj%PoissonRatio(numDomain) )
		allocate(obj%Density(numDomain) )
		obj%YoungModulus(:) = DefaultYoungModulus
		obj%PoissonRatio(:) = DefaultPoissonRatio
		obj%Density(:) = DefaultDensity
		


		! receive domains as pointers
		do i=1,numDomain
			obj%femdomains(i)%femdomainp => femdomainsp(i)%femdomainp
		enddo

		obj%ContactList = contactList


		! initialize solver
		allocate(NumberOfNode(numDomain))
		do i=1,numDomain
			NumberOfNode(i) = obj%femdomains(i)%femdomainp%nn()
		enddo
		call obj%solver%init(NumberOfNode=NumberOfNode, DOF = obj%femdomains(1)%femdomainp%nd() )

		obj%initialized = .true.
		return
	
	else
		print *, "[Caution] :: contactmechanics%init >> you attempt to run REGACY mode. If you want to run"
		print *, "Modern version, please set your type(FEMDomain_),allocatable :: something(:) object as"
		print *, "femdomains = "
	endif



	if(present(femdomain1) )then
		! regacy
		! regacy
		! regacy
		! regacy
	    if(allocated(obj%KcontactEBE) )then
	        deallocate(obj%KcontactEBE)
	    endif
	    if(allocated(obj%KcontactGlo) )then
	        deallocate(obj%KcontactGlo)
	    endif
	    if(allocated(obj%FcontactEBE) )then
	        deallocate(obj%FcontactEBE)
	    endif
	    if(allocated(obj%FcontactGlo) )then
	        deallocate(obj%FcontactGlo)
	    endif
	    if(allocated(obj%DispVecEBE) )then
	        deallocate(obj%DispVecEBE)
	    endif
	    if(allocated(obj%DispVecGlo) )then
	        deallocate(obj%DispVecGlo)
	    endif
	    if(allocated(obj%NTSvariables) )then
	        deallocate(obj%NTSvariables)
	    endif

		if(associated(obj%femdomain1) )then
			nullify(obj%femdomain1)
		endif
		if(associated(obj%femdomain2) )then
			nullify(obj%femdomain2)
		endif

		obj%femdomain1 => femdomain1
		obj%femdomain2 => femdomain2
	    !if(.not. associated(obj%FEMDomain1) )then
	    !    print *, "ContactMechanics%Init >> FEMDomain1 is not imported"
	    !    return
	    !endif
	    !if(.not. associated(obj%FEMDomain2) )then
	    !    print *, "ContactMechanics%Init >> FEMDomain2 is not imported"
	    !    return
	    !endif
	    !if(.not. associated(obj%FEMIface) )then
	    !    print *, "ContactMechanics%Init >> FEMIface is not imported"
	    !    return
		!endif
		if(obj%femdomain1%mesh%empty()  .eqv. .true.)then
			print *, "[Caution] >> initContactMechanics:: obj%femdomain1%mesh is empty" 
			stop
		endif
		if(obj%femdomain2%mesh%empty()  .eqv. .true.)then
			print *, "[Caution] >> initContactMechanics:: obj%femdomain2%mesh is empty" 
			stop
		endif

		node_num_1 = size(obj%femdomain1%mesh%nodcoord,1)
		node_num_2 = size(obj%femdomain2%mesh%nodcoord,1)

		! initialize data objects
		if(.not. allocated(obj%duvec))then
			allocate(obj%duvec((node_num_1+node_num_2)*size(obj%femdomain1%mesh%nodcoord,2) ))
		endif
		if(.not. allocated(obj%uvec))then
			allocate(obj%uvec((node_num_1+node_num_2)*size(obj%femdomain1%mesh%nodcoord,2) ))
		endif
		if(.not. allocated(obj%dfvec))then
			allocate(obj%dfvec((node_num_1+node_num_2)*size(obj%femdomain1%mesh%nodcoord,2) ))
		endif

		if(.not. allocated(obj%fvec))then
			allocate(obj%fvec((node_num_1+node_num_2)*size(obj%femdomain1%mesh%nodcoord,2) ))
		endif
		return
	endif
end subroutine
! #####################################################
subroutine fixContactMechanics(obj,direction,disp,DomainID,x_min,x_max,y_min,y_max,z_min,z_max,NodeiDs,reduction)
	class(ContactMechanics_),intent(inout) :: Obj
	character(1),intent(in) :: direction
	real(real64),intent(in) :: disp
	integer(int32),intent(in) :: DomainID
	integer(int32),optional,intent(in) :: NodeIDs(:)
	real(real64),optional,intent(in) :: reduction ! percent
	real(real64),optional,intent(in) :: x_min,x_max,y_min,y_max,z_min,z_max
	real(real64) :: rate
	integer(int32),allocatable :: FixBoundary(:),reducedFixBoundary(:)
	integer(int32) :: entryID,i,nsize,interval

	print *, "fixContactMechanics >> [1] selecting fix boundary"
	if(present(NodeIDs) )then
		FixBoundary = NodeIDs
	else
		FixBoundary = obj%FEMdomains(DomainID)%femdomainp%select(&
			x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)
	endif

	if(direction=="x")then
		EntryId=1
	endif
	if(direction=="y")then
		EntryId=2
	endif
	if(direction=="z")then
		EntryId=3
	endif
	if(direction=="X")then
		EntryId=1
	endif
	if(direction=="Y")then
		EntryId=2
	endif
	if(direction=="Z")then
		EntryId=3
	endif

	

!	if(present(reduction) )then
!		print *, "fixContactMechanics >> [2] setting fix boundary, size:: ",nsize
!		if( 0.0d0 < reduction .and. reduction < 1.0d0  )then
!			rate = reduction
!		elseif( 1.0d0 < reduction .and. reduction < 100.0d0  )then
!			rate = reduction/100.0d0
!		endif
!		nsize = int( dble(size(FixBoundary) )*rate )
!		interval = int(size(FixBoundary)/nsize)
!
!		do i=1,size(FixBoundary),interval
!			call obj%solver%fix(nodeid=FixBoundary(i), &
!				EntryID=EntryID, &
!				entryvalue=disp, &
!				DOF=obj%solver%DOF ,&
!				row_DomainID=domainid)
!		enddo
!	else
!		print *, "fixContactMechanics >> [2] setting fix boundary, size:: ",size(FixBoundary)
!		do i=1,size(FixBoundary)
!			call obj%solver%fix(nodeid=FixBoundary(i), &
!				EntryID=EntryID, &
!				entryvalue=disp, &
!				DOF=obj%solver%DOF ,&
!				row_DomainID=domainid)
!		enddo
!	endif


	print *, "fixContactMechanics >> [2] setting fix boundary, size:: ",size(FixBoundary)

	do i=1,size(FixBoundary)
		call obj%solver%fix(nodeid=FixBoundary(i), &
			EntryID=EntryID, &
			entryvalue=disp, &
			DOF=obj%solver%DOF ,&
			row_DomainID=domainid)
	enddo

	print *, "fixContactMechanics >> [ok] Done"

end subroutine
! #####################################################
subroutine updateMeshContactMechanics(obj)
	class(ContactMechanics_),target,intent(inout) :: obj
	integer(int32) :: i,DOF,From,To

	if(obj%initialized )then
		DOF = obj%solver%DOF
		From = 1
		To   = 0
		do i=1,size(obj%solver%NumberOfNode)
			To   = To +  obj%solver%NumberOfNode(i)*DOF
			obj%femdomains(i)%femdomainp%mesh%nodcoord(:,:) = obj%femdomains(i)%femdomainp%mesh%nodcoord(:,:) +&
    			reshape( obj%solver%x(From:To),obj%femdomains(i)%femdomainp%nn(),DOF )
			From = From + obj%solver%NumberOfNode(i)*DOF
		enddo
	endif
end subroutine

! #####################################################


! #####################################################
subroutine runCM(obj,penaltyparameter,debug,GaussPointProjection)
	class(ContactMechanics_),target,intent(inout) :: obj
	real(real64),optional,intent(in) :: penaltyparameter

	logical,optional,intent(in) :: debug
	logical :: Debugflag=.false.
	logical,optional,intent(in) :: GaussPointProjection
	integer(int32) :: i,nod_max,nn,itr,fstep,j,k,l,o,GaussPointID
	integer(int32) :: node_num_1,node_num_2,converge_check,error
	type(IO_) :: ErrorLog,f
	real(real64) :: rvec0,u_norm,er,er0,reacforcex,reacforcey
	
	integer(int32) :: DomainID, ElementID,InterfaceID,NodeID
	integer(int32),allocatable :: DomainIDs1(:),DomainIDs12(:),InterConnect(:)

	real(real64),allocatable :: A_ij(:,:), x_i(:), b_i(:) ! A x = b
	real(real64),allocatable :: A_ij_GPP(:,:)
	real(real64) :: position(3),center(3)
	real(real64) :: penalty
	type(FEMDomain_),pointer :: domain1, domain2
	type(ShapeFunction_) :: sf
	logical :: GPP ! enable Gauss-Point projection
	if(present(GaussPointProjection) )then
		if(GaussPointProjection)then
			GPP=.true.
		endif
	endif


	if(present(debug) )then
		obj%solver%debug = debug
	endif

	if( obj%initialized  )then
		
		! linear elastic, small strain 
		! create stiffness matrix for all domains
		do DomainID=1, size(obj%femdomains)
			print *, "Ax = b for Domain-ID :: ",DomainID 
			if(allocated(DomainIDs1 ))then
				deallocate(DomainIDs1)
			endif

			allocate(DomainIDs1(obj%femdomains(DomainID)%femdomainp%nne()&
				*obj%femdomains(DomainID)%femdomainp%nd() ) )
			
			DomainIDs1(:) = DomainID
			
			do ElementID=1, obj%femdomains(DomainID)%femdomainp%ne()
				
				! For 1st element, create stiffness matrix
			    A_ij = obj%femdomains(DomainID)%femdomainp%StiffnessMatrix(&
					ElementID=ElementID,&
					E=obj%YoungModulus(DomainID), &
					v=obj%PoissonRatio(DomainID))
			    b_i  = obj%femdomains(DomainID)%femdomainp%MassVector(&
			        ElementID=ElementID,&
			        DOF=obj%femdomains(DomainID)%femdomainp%nd() ,&
			        Density=obj%Density(DomainID),&
			        Accel=obj%Gravity&
			        )
			    ! assemble them 
			    call obj%solver%assemble(&
			        connectivity=obj%femdomains(DomainID)%femdomainp%connectivity(ElementID=ElementID),&
			        DOF=obj%femdomains(DomainID)%femdomainp%nd() ,&
			        eMatrix=A_ij,&
			        DomainIDs=DomainIDs1)
			    call obj%solver%assemble(&
			        connectivity=obj%femdomains(DomainID)%femdomainp%connectivity(ElementID=ElementID),&
			        DOF=obj%femdomains(DomainID)%femdomainp%nd(),&
			        eVector=b_i,&
			        DomainIDs=DomainIDs1)
			enddo
		enddo

		InterfaceID = 0
		penalty = input(default=obj%penalty,option=penaltyparameter)
		do i=1, size(obj%ContactList,1)
			do j=1, size(obj%ContactList,2)
				if(obj%contactList(i,j)>=1 )then
					! domains are in contact
					InterfaceID = InterfaceID+1
					! Interface
					print *, "K_c x = 0 for Interface ID :: ",InterfaceID

					! create Elemental Matrices and Vectors
					if(allocated(DomainIDs12) )then
						deallocate(DomainIDs12)
					endif
					if(allocated(InterConnect) )then
						deallocate(InterConnect)
					endif
					if(associated(domain1) )then
						nullify(domain1)
					endif
					if(associated(domain2) )then
						nullify(domain2)
					endif

					domain1 => obj%femdomains(i)%femdomainp
					domain2 => obj%femdomains(j)%femdomainp


					allocate(DomainIDs12(domain2%nne()+1 ) )
					allocate(InterConnect(domain2%nne()+1 ) )
					
					DomainIDs12(1) = i
					DomainIDs12(2:) = j


					if(GPP)then
						! compute constrait matrix 
						! by Gauss-Point Projection
						InterConnect = int( zeros(domain1%nne()+ domain2%nne()) )
						
						DomainIDs12 = int( zeros(domain1%nne()+ domain2%nne()) ) 
						DomainIDs12(1:domain1%nne() ) = i
						DomainIDs12(domain1%nne()+1: ) = j

						do ElementID=1, domain1%ne()
							do GaussPointID = 1, domain1%ngp()
			
								! For 1st element, create stiffness matrix
						    	! set global coordinate

								position = domain1%GlobalPositionOfGaussPoint(ElementID,GaussPointID)
			
								
								
						    	if( domain2%mesh%nearestElementID(x=position(1),y=position(2),z=position(3))<=0 )then
						    	    cycle
						    	endif
								
								InterConnect(1:domain1%nne() ) = domain1%connectivity(ElementID)
						    	InterConnect(domain1%nne()+1:) &
									= domain2%connectivity(domain2%mesh%nearestElementID(x=position(1),y=position(2),z=position(3) ))
								

								sf = domain1%mesh%getShapeFunction(ElementID,GaussPointID)
								!print *, "shapefunc"
								!call print(matmul( transpose(sf%ElemCoord),sf%nmat) )

								sf%ElementID=ElementID
								
								A_ij = penalty*domain2%connectMatrix(position,DOF=domain2%nd(),shapefunction=sf) 
								!A_ij = penalty*domain2%connectMatrix(position,DOF=domain2%nd()) 
						    	
								!call f%open("Domain1.txt")
								!!call f%write(A_ij)
								!call f%close()
								!call f%open("Domain1.txt")
								!!call f%write(A_ij)
								!call f%close()
								
								!stop
								!stop
								
								! assemble them 
						    	call obj%solver%assemble(&
						    	    connectivity=InterConnect,&
						    	    DOF=domain2%nd() ,&
						    	    eMatrix=A_ij,&
						    	    DomainIDs=DomainIDs12)    
							enddo
						enddo
					else
						do NodeID=1, domain1%nn()
						    ! For 1st element, create stiffness matrix
						    ! set global coordinate
							position(:) = domain1%mesh%nodcoord(NodeID,:)
						    if( domain2%mesh%nearestElementID(x=position(1),y=position(2),z=position(3))<=0 )then
						        cycle
						    endif
							InterConnect(1) = NodeID
						    InterConnect(2:) = domain2%connectivity(domain2%mesh%nearestElementID(x=position(1),y=position(2),z=position(3) ))
						    A_ij = penalty*domain2%connectMatrix(position,DOF=domain2%nd() ) 
						    ! assemble them 
						    call obj%solver%assemble(&
						        connectivity=InterConnect,&
						        DOF=domain2%nd() ,&
						        eMatrix=A_ij,&
						        DomainIDs=DomainIDs12)    
						enddo
					endif
				endif
			enddo
		enddo

		call obj%solver%prepareFix()

		return




	else
		print *, "[Caution] :: runContactMechanics >> No domain was found in femdomains=***"
		print *, "You attempt to run it as REGACY mode."	

		if(present(debug) )then
			Debugflag = debug
		endif
		! initialize domains as deformable bodies
		call obj%femdomain1%bake(template="FiniteDeform_")
		call obj%femdomain2%bake(template="FiniteDeform_")

		! get displacement boundary
		call obj%getDispBound()		
		call obj%getTracBound()		



		if(obj%control == 2) then
			!外力制御であれば、外力増分の計算
			do i = 1, size(obj%dfvec)
				obj%dfvec(i) = 1.0d0/dble(obj%timestep)*obj%fvec(i)
			enddo
			obj%fvec(:) = 0.0d0

		elseif(obj%control == 1) then
			!変位制御であれば、変位増分の計算と外力ベクトルの計算

			obj%du_nod_dis_x(:)= 1.0d0/dble(obj%timestep)*obj%u_nod_dis_x(:)
			obj%du_nod_dis_y(:)= 1.0d0/dble(obj%timestep)*obj%u_nod_dis_y(:)
			obj%u_nod_dis_x(:)= 0.0d0
			obj%u_nod_dis_y(:)= 0.0d0	
		
		endif


		call ErrorLog%open("Contact_ErrorLog.txt")
		write(ErrorLog%fh,*)'step=',1,"/",fstep
		obj%step=0

		! time-loop
		do i=1, obj%TimeStep
			obj%step = obj%step+1
			obj%duvec(:) = 0.0d0
			obj%itr_contact = 0

			fstep=obj%TimeStep
			print *, 'Step=',i !現在のstepの出力
			if(Debugflag .eqv. .true.) print *, "Debug flag 0"


			!=========================================================
			!Add force/displacement increments
			!--------------------------------------
			obj%itr = 0 !N-R法ループ1回目

			if(obj%control ==1) then  !変位/外力増分の追加
				call obj%ls_add_du() !強制変位量の追加
			elseif(obj%control==2) then
				obj%fvec(:)=obj%fvec(:)+obj%dfvec(:) !外力増分の追加
			else
				print *,"wrong nomber is in control"
				exit
			endif
			!==================================================================
			if(Debugflag .eqv. .true.) print *, "Debug flag 1"

			call obj%ls_nts_generate()
			call obj%ls_get_stabilized_nts()
			call obj%ls_nts_material()
			!================================================================
			if(Debugflag .eqv. .true.) print *, "Debug flag 2"


			if(obj%nts_elem_nod(1,1)+obj%nts_elem_nod(1,2)+obj%nts_elem_nod(1,3)/=0 )then !contact exists
				!================================================================
				!check for contact: gn<0 → active NTS-element
				!--------------------------------------------
				call obj%ls_check_active()
				!================================================================
			endif
			if(Debugflag .eqv. .true.) print *, "Debug flag 3"
		

			!===============================================================================
			!Elastic stick の計算(trial phase)
			!Calculate [K_stick(u)],[K(u)],gvec
			!-----------------------------------
			if(.not.allocated(obj%k_contact) )	then
				allocate(obj%k_contact(size(obj%uvec),size(obj%uvec) ) )
			endif
			if(.not.allocated(obj%fvec_contact) )	then
				allocate(obj%fvec_contact(size(obj%uvec) ) )
			endif
			obj%k_contact(:,:)=0.0d0 
			obj%fvec_contact(:)=0.0d0


			if(obj%nts_elem_nod(1,1)+obj%nts_elem_nod(1,2)+obj%nts_elem_nod(1,3)/=0 )then !contact exists
			!nts諸量の初期化
				allocate(obj%nts_amo(size(obj%nts_elem_nod,1),12),obj%stick_slip( size(obj%nts_elem_nod,1) )  )
				obj%nts_amo(:,:)=0.0d0
				obj%stick_slip(:)=0
				!もし過去にNTSを構成していれば、load data
				if(allocated(obj%old_nts_amo))then
					call load_nts_element(obj%nts_elem_nod,obj%nts_amo,obj%old_nts_elem_nod,obj%old_nts_amo,&
						obj%stick_slip,obj%old_stick_slip)
				endif
				obj%stick_slip(:)=0
				do j = 1, size(obj%active_nts,1)

					if(obj%stick_slip(obj%active_nts(j) )==0 )then
						nod_max=size(obj%nod_coord,1)
						call state_stick(j,nod_max,obj%nod_coord,obj%nts_elem_nod,obj%active_nts&
						,obj%nts_amo, obj%k_contact,obj%nts_mat,obj%contact_mat_para,obj%uvec,obj%fvec_contact,&
						obj%stick_slip)  !state stick and K_contactへの重ね合わせ

					else
						call update_res_grad_c_i(j,nod_max,obj%nod_coord,obj%nts_elem_nod,obj%active_nts&
							,obj%nts_amo,obj% k_contact,obj%uvec,obj%duvec,obj%fvec_contact,obj%stick_slip,&
							obj%contact_mat_para,obj%nts_mat) 
					endif
				enddo

			endif
		

			! ここは、FiniteDeformationClassから呼び出し


			obj%kmat(:,:)=0.0d0
			obj%gvec(:)=0.0d0
			!call k_mat_f_int(elem_mat,elem_nod,f_nod,nod_coord,mat_cons, Kmat,stress,duvec,&
			!		pulout,gvec,sigma,uvec,strain_measure,itr_tol,tol,itr,i,obj%itr_contact)
			!================================================================================
			if(Debugflag .eqv. .true.) print *, "Debug flag 4"


			!==========================================================================
			!Solve
			!-----------------------
			obj%K_total(:,:) = obj%kmat(:,:)+ obj%k_contact(:,:) !全体接触剛性マトリクスの計算
			obj%rvec(:)=obj%fvec(:)-obj%gvec(:)-obj%fvec_contact(:)!fvec_contact(:)
			!!!!no tension wall 保留中
			!call  no_tension_wall(gvec,surface_nod,sur_nod_inf,nod_coord,uvec,&
			!	u_nod_x,u_nod_y,active_wall_x,active_wall_y)
			!active_wall_x(:)=1
			!active_wall_y(:)=1
			!==================

			call displace(obj%K_total, obj%rvec, obj%u_nod_x, obj%du_nod_dis_x,obj%u_nod_y, &
				obj%du_nod_dis_y) !Dirichlet Boundary conditions
			nn=size(obj%uvec,1)    !Parameters for gauss_joprdan

			do k=1,size(obj%rvec)
				if(obj%rvec(k)>=0.0d0 .or. obj%rvec(k)<0.0d0 )then
					cycle
				else
					error=1
					print *, "NaN !!"
					exit
				endif
			enddo

			do k=1,size(obj%K_total,1)
				do l=1,size(obj%k_total,2)
					if(obj%K_total(k,l) >=0.0d0 .or. obj%K_total(k,l)<0.0d0 )then
						cycle
					else
						error=1
						print *, "NaN !!"
						exit
					
					endif
				enddo
			enddo			
		
			!call gauss_jordan_pv(k_total, duvec, rvec, nn)
			!duvec(:)=0.0d0
			!call bicgstab1d(k_total, Rvec, duvec, nn, itr_tol, tol_rm)	!obtain initial du
		
			er=1.0e-15
			nn = size(obj%rvec)
			call bicgstab_nr1(obj%k_total, obj%Rvec, obj%duvec, nn, obj%BiCG_ItrMax,&
				er,obj%u_nod_x, obj%u_nod_y,obj%u_nod_dis_x,obj%u_nod_dis_y)



			!#### ERROR CHECKER ########
			if(dot_product(obj%duvec,obj%duvec) /=dot_product(obj%duvec,obj%duvec)  )then
				print *, "ERROR :: runContactMechanics"
				exit
			endif
			!#### ERROR CHECKER ########



			!call gnuplot_out(elem_nod,nod_coord,uvec+duvec,i,process_parallel)
			! stop  "debug"

			!x=duvec(2*u_nod_y(1))
			!write(108,*) x,du_nod_dis_y(1)
			!if(int(x)/=int(du_nod_dis_y(1)) )then
			!	error=1
			!	print *, "invalid uvec"
			!	exit
			!endif
			obj%initial_duvec(:)=obj%duvec(:)

			!=========================================================================
			print *, "Debug flag 5"


			do 


				!call gnuplot_out(elem_nod,nod_coord,uvec+duvec,obj%itr_contact,process_parallel)

				!==================================================================
				if(obj%nts_elem_nod(1,1)+obj%nts_elem_nod(1,2)+obj%nts_elem_nod(1,3)/=0  )then !contact exists
					!==================================================================
					!check contact pairing
				

					!==================================================================
					!check for contact: gn<0 → active NTS-element
					!-----------------------------------------------
					call obj%ls_check_active()
					!=====================================================================
				endif
				print *, "Debug flag 6"			



				!================================================

				if(obj%nts_elem_nod(1,1)+obj%nts_elem_nod(1,2)+obj%nts_elem_nod(1,3)/=0 .and. obj%itr_contact>=2 )then !contact exists
					obj%k_contact(:,:)=0.0d0	
					obj%fvec_contact(:)=0.0d0

					do j = 1, size(obj%active_nts,1)
						call update_friction(j,nod_max,obj%nod_coord,obj%nts_elem_nod,obj%active_nts,obj%surface_nod,obj%sur_nod_inf&
						,obj%nts_amo,obj% k_contact,obj%uvec,obj%duvec,obj%fvec_contact,obj%stick_slip,&
						obj%contact_mat_para,obj%nts_mat,obj%itr_contact) !with return mapping Algorithm
					enddo

				endif

			

				!================================================
				print *, "Debug flag 7"

				!====================================================================
				!LOOP OVER ITERATIONS : k = 1, 2, ..., convergence
				!------------------------------------------------------

				itr = itr + 1 
				obj%dduvec(:)=0.0d0

				if(obj%nts_elem_nod(1,1)+obj%nts_elem_nod(1,2)+obj%nts_elem_nod(1,3)/=0 )then !contact exists
					obj%k_contact(:,:)=0.0d0	
					obj%fvec_contact(:)=0.0d0

					do j = 1, size(obj%active_nts,1)
						call update_res_grad_c(j,nod_max,obj%nod_coord,obj%nts_elem_nod,obj%active_nts&
						,obj%nts_amo,obj% k_contact,obj%uvec,obj%duvec,obj%fvec_contact,obj%stick_slip,&
						obj%contact_mat_para,obj%nts_mat) !with return mapping Algorithm
					enddo

				endif
			
			
				!===============================================================================
				!Elastic stick/ Plastic slip の計算
				!Calculate [K_stick(u)],[K(u)] 
				!-----------------------------------
				obj%kmat(:,:)=0.0d0
				obj%gvec(:)=0.0d0

				!call k_mat_f_int(elem_mat,elem_nod,f_nod,nod_coord,mat_cons, Kmat,stress,duvec,pulout,gvec,&
				!	sigma,uvec,strain_measure,itr_tol,tol,itr,i,obj%itr_contact)

				!================================================================================
				print *, "Debug flag 8"

					!================================================================================
					!Calculate Rresidual vecor r
					!--------------------------------
				obj%k_total(:,:)=obj%kmat(:,:)+obj%k_contact(:,:)
				obj%rvec(:)=obj%fvec(:)-obj%gvec(:)-obj%fvec_contact(:)
				if(itr==1)then
					rvec0=abs(dot_product(obj%rvec,obj%rvec))   
					!**(1.0d0/2.0d0)
				endif
				!=================================================================================
				print *, "Debug flag 9"
				print *, "itr=",itr

			
				!================================================================================
				!Solve
				!---------------

				!!!!no tension wall 保留中
				!call  no_tension_wall(gvec,surface_nod,sur_nod_inf,nod_coord,uvec+duvec,&
				!u_nod_x,u_nod_y,active_wall_x,active_wall_y)
				!if(obj%itr_contact<0)then
					!active_wall_x(:)=1
					!active_wall_y(:)=1
				!endif
				!=====================
			
				call displace_nr(obj%K_total, obj%Rvec, obj%u_nod_x, obj%u_nod_dis_x,obj%u_nod_y, &
					obj%u_nod_dis_y) !変位境界ではΔu=0
				

				!call gauss_jordan_pv(k_total, dduvec, Rvec, nn)
				!call bicgstab1d(k_total, Rvec, dduvec, nn, itr_tol, tol_rm)
				obj%dduvec(:)=0.0d0
				er=1.0e-15
				
				!NaN checker
				do k=1,size(obj%rvec)
					if(obj%rvec(k)>=0.0d0 .or. obj%rvec(k)<0.0d0 )then
						cycle
					else
						error=1
						exit
					endif
				enddo

				do k=1,size(obj%K_total,1)
					do l=1,size(obj%k_total,2)
						if(obj%K_total(k,l) >=0.0d0 .or. obj%K_total(k,l)<0.0d0 )then
							cycle
						else
							error=1
							exit
						endif
					enddo
				enddo

				call bicgstab_nr(obj%k_total, obj%Rvec, obj%dduvec, nn, obj%BiCG_ItrMax,er,&
					obj%u_nod_x, obj%u_nod_y)




				!#### ERROR CHECKER ########
				if(dot_product(obj%dduvec,obj%dduvec) /=dot_product(obj%dduvec,obj%dduvec)  )then
					error=1
					exit
				endif

				!#### ERROR CHECKER ########



				!---変位ベクトルの足しこみ-----------------------------
				obj%duvec(:) = obj%duvec(:) + obj%dduvec(:) 

			

				if(obj%itr_contact*itr==1)then
					obj%dduvec_nr(:)=obj%dduvec(:)
				endif




				u_norm=abs(dot_product(obj%rvec,obj%rvec))/rvec0!
				if(u_norm==0.0d0)then
					print *, "u_norm=0 at step",i,"contact_itr=",obj%itr_contact,"itr=",itr
				endif
				!u_norm=(abs(dot_product(rvec,rvec))**(1.0d0/2.0d0))/u_norm
				!#### ERROR CHECKER ########
				if(u_norm>100000.0d0  )then
					error=1
				endif


			
				if(error==1)then
					exit
				endif
				!#### ERROR CHECKER ########
				!==========================================================================
				!Check convergence
				!Contact analysisの収束判定
				!------------------------------------			

				!call gnuplot_out(elem_nod,nod_coord,uvec+duvec,itr,process_parallel)
				if( abs(u_norm) <= 1.0e-5 .and. obj%itr_contact>=2 ) then
				
					print *, 'contact loop itr=',itr,'residual_out_cont',u_norm
					!write(1000,*)'contact loop itr=',itr,'residual_out_cont',u_norm
					obj%uvec(:)=obj%uvec(:)+obj%duvec(:)
					if(dot_product(obj%uvec,obj%uvec)==0.0d0 )then
						error=1
						exit
					endif
					obj%duvec(:)=0.0d0
					!compute traction forces:
					reacforcey=0.0d0
					reacforcex=0.0d0
					do o =1, size(obj%u_nod_x)
						if(obj%u_nod_dis_x(o)==0.0d0)then
							cycle
						else
							reacforcex=reacforcex+obj%gvec(2*obj%u_nod_x(o)-1)
						endif
					enddo
					do o =1, size(obj%u_nod_y)
						if(obj%u_nod_dis_y(o)==0.0d0)then
							cycle
						else
							reacforcey=reacforcey+obj%gvec(2*obj%u_nod_y(o))
						endif
					enddo

					!if(i==outputstep)then
					!	outputstep=outputstep+ops
					!	!call gnuplot_out(elem_nod,nod_coord,uvec,i,process_parallel)
					!endif
					!call gnu_st(elem_nod,nod_coord,uvec,sigma,i,scalar)

					!write(1000,*)'step=',i+1,"/",fstep
					print *, "Debug flag 11"
					!debug
					!write(52,*) "step, s12 elem,gauss=(1,1),(2,1)...",Fstep, sigma(:,:,3)
					
					if(obj%nts_elem_nod(1,1)+obj%nts_elem_nod(1,2)+obj%nts_elem_nod(1,3)/=0 )then !contact exists
						call save_nts_element(obj%nts_elem_nod,obj%nts_amo,obj%old_nts_elem_nod,obj%old_nts_amo,obj%surface_nod,obj%sur_nod_inf,obj%&
						stick_slip,obj%old_stick_slip)
						deallocate(obj%nts_amo,obj%active_nts,obj%stick_slip)
					endif
					!call output_stress_contour(nod_coord,uvec,elem_nod,sigma,strain_measure,i,process_parallel )
					deallocate(obj%nts_elem_nod)
					!call variable_update(strain_measure)
					!converge_check=1
					!error=0

					exit
				elseif( abs(u_norm) <= obj%Tolerance .and. obj%itr_contact<=1 ) then

					obj%itr_contact=obj%itr_contact+1
					itr=0
				
					cycle

				else
				
					print *, 'contact loop itr=',obj%itr_contact,'residual_out',u_norm
					!write(1000,*)'contact loop itr=',obj%itr_contact,'residual_out',u_norm


					if(itr >= obj%NR_ItrMax)then
						!close(40)
						!close(50)
						!close(61)
						!close(70)					
						!call execute_command_line("png_script.gp")
						!call execute_command_line("stre_png_scr.gp")
						! stop 'contact loop did not converge'
						print *, "ERROR :: NR-did not converge"
						converge_check=0

						exit
					else
					
						cycle
					
					endif

				endif
				!------------収束判定ここまで------------------------------------------------
				!===========================================================================



			enddo

			if(converge_check==0 .or. error ==1 )then
				exit
			endif

		enddo
	
		!output restart data
		if(error==0 .and. converge_check==1)then 

			!call restart_out(nod_coord,uvec,fvec,sigma,strain_measure,old_nts_elem_nod,&
		!		old_nts_amo,old_stick_slip,gvec,process_parallel)
		endif
		write(*,*)'Contact Elasto-Plastic analysis was completed!'

		call ErrorLog%close()
	endif

end subroutine

! #####################################################
subroutine propertiesCM(obj,config,penalty,gravity)
	class(ContactMechanics_),intent(inout) :: obj
	character(*),optional,intent(in) :: config
	real(real64),optional,intent(in) :: penalty,gravity(3)
	type(IO_) :: f
	character(200) :: fn,conf,line
	integer(int32) :: blcount,rmc,id

	if(present(penalty) )then
		obj%penalty = penalty
	endif

	if(present(gravity) )then
		obj%gravity = gravity
	endif

	if(.not.present(config).or. index(config,".json")==0 )then
		
        print *, "New contact-configuration >> contact.json"
		call f%open("contact.json")
		
        write(f%fh,*) '{'
        write(f%fh,*) '   "type": "contact",'
		write(f%fh,*) '    "FrictionalCoefficient": 0.30,'
		write(f%fh,*) '    "PenaltyParameter": 1.0e+5,'
		write(f%fh,*) '    "Cohesion": 0.00,'
		write(f%fh,*) '    "TimeStep": 100,'
        write(f%fh,*) '    "Tolerance:":1.0e-10,'
        write(f%fh,*) '    "BiCG_ItrMax:":10000'
        write(f%fh,*) '}'

		conf="contact.json"

		call f%close()
    else
        conf = trim(config)
	endif

    call f%open(trim(conf))
    blcount=0
    do
        read(f%fh,'(a)') line
        print *, trim(line)
		
		if( adjustl(trim(line))=="{" )then
            blcount=1
            cycle
		endif
		
        if( adjustl(trim(line))=="}" )then
            exit
        endif
        
        if(blcount==1)then
            
            if(index(line,"FrictionalCoefficient")/=0)then
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%FrictionalCoefficient
			endif
			
			if(index(line,"TimeStep")/=0)then
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%TimeStep
			endif

			
			if(index(line,"Tolerance")/=0)then
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%Tolerance
			endif
			
			if(index(line,"PenaltyParameter")/=0)then
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%penaltypara
			endif

			if(index(line,"Cohesion")/=0)then
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%Cohesion
			endif


			if(index(line,"BiCG_ItrMax")/=0)then
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%BiCG_ItrMax
			endif
		endif
	enddo
	call f%close()

	if(allocated(obj%sur_inf_mat)) deallocate(obj%sur_inf_mat)
	if(allocated(obj%contact_mat)) deallocate(obj%contact_mat)
	if(allocated(obj%contact_mat_para)) deallocate(obj%contact_mat_para)
	
	allocate(obj%sur_inf_mat(1,3))
	allocate(obj%contact_mat(1,1))
	allocate(obj%contact_mat_para(1,4))

	! from surface nod id
	obj%sur_inf_mat(1,1)=1
	! to 
	obj%sur_inf_mat(1,2)=size(obj%femdomain1%mesh%nodcoord,1)+size(obj%femdomain2%mesh%nodcoord,1)
	obj%sur_inf_mat(1,3)=1 !; material id = 1

	obj%contact_mat(:,:)=1

	obj%contact_mat_para(1,1) =obj%penaltypara ! eT
	obj%contact_mat_para(1,2) =obj%penaltypara ! eN
	obj%contact_mat_para(1,3) =obj%Cohesion ! c
	obj%contact_mat_para(1,4) =atan(obj%FrictionalCoefficient) ! Φ

end subroutine
! #####################################################


! #####################################################
subroutine UpdateContactConfiguration(obj,WeakCoupling,StrongCoupling)
	class(ContactMechanics_),intent(inout)::obj
	logical,optional,intent(in) :: WeakCoupling,StrongCoupling
	!type(MPI_)::mpidata



	
	if( present(WeakCoupling))then
		if(WeakCoupling .eqv. .true.)then


			! only 3-D is supported.

			call obj%FEMIface%GetFEMIface()
			call obj%deploy(obj%FEMIface)
			call obj%setPenaltyParameter( dble(1.0e-4) )
			call obj%updateContactStress()
			call obj%updateTimeStep()
			

			! debug :: Contact-Traction conversion has errors
			
			call obj%FEMDomain1%export(OptionalProjectName="1ontact_1_",FileHandle=120,SolverType="FiniteDeform_",MeshDimension=3)
			call obj%FEMDomain2%export(OptionalProjectName="2ontact_2_",FileHandle=121,SolverType="FiniteDeform_",MeshDimension=3)
			
			
			!call mpidata%end()
			stop "debug update contact"
			! debug :: Contact-Traction conversion has errors

			
			return
		endif
	endif

	if( present(StrongCoupling))then
		if(StrongCoupling .eqv. .true.)then
			
			! only 2-D is supported.
			call obj%FEMIface%GetFEMIface()
			call obj%deploy(obj%FEMIface)
			call obj%setPenaltyParameter( dble(1.0e-4) )
			
			print *, "Debugging ls25"
			return
			
			call obj%updateContactStress()
			call obj%updateTimeStep()
			
			! debug :: Contact-Traction conversion has errors
			
			call obj%FEMDomain1%export(OptionalProjectName="1ontact_1_",FileHandle=120,SolverType="FiniteDeform_",MeshDimension=2)
			call obj%FEMDomain2%export(OptionalProjectName="2ontact_2_",FileHandle=121,SolverType="FiniteDeform_",MeshDimension=2)
			
			! debug :: Contact-Traction conversion has errors

			return
		endif
	endif

	
end subroutine
! #####################################################




! #####################################################
subroutine ImportContactMechanics(obj)
    class(ContactMechanics_),intent(inout)::obj
    

end subroutine
! #####################################################

! #####################################################
subroutine deployContactMechanics(obj,IfaceObj)
	class(ContactMechanics_),intent(inout)::obj
	class(FEMIface_),target,intent(in)::IfaceObj

	obj%FEMIface => IfaceObj

end subroutine
! #####################################################


! #####################################################
subroutine ContactSearch(obj)
    class(ContactMechanics_),intent(inout)::obj

    integer :: ierr,i,n

	
	call obj%FEMIface%GetFEMIface(obj%FEMDomain1,obj%FEMDomain2)
	
	
	call GetActiveContactElement(obj)

end subroutine
! #####################################################


! #####################################################
subroutine GetActiveContactElement(obj)
    class(ContactMechanics_),intent(inout)::obj

    ! Check Active/Incative of contact elements
    ! For Node-To-Segment
    call GetActiveNTS(obj)

end subroutine
! #####################################################



! #####################################################
subroutine GetActiveNTS(obj)
    class(ContactMechanics_),intent(inout)::obj
	!type(MPI_)::mpidata
    real(real64) :: gap
    real(real64),allocatable :: xs(:),xm(:,:)
	integer i,j,n,dim_num,mnod_num
	

    
    dim_num = size(obj%FEMDomain1%Mesh%NodCoord,2)
    if(dim_num < 1 .or. dim_num >4)then
        print *, "ContactMechanics_ >> GetActiveNTS >> dim_num should be 2 or 3 "
        stop 
    endif
    mnod_num = size(obj%FEMIface%NTS_NodCoord,2)/dim_num-1
    allocate(xs(dim_num),xm(mnod_num,dim_num))



    do i=1,size(obj%FEMIface%NTS_NodCoord,1) ! NTS_NodeCoordID
        xs(1:dim_num)=obj%FEMIface%NTS_NodCoord(i,1:dim_num)
		
		do j=1,mnod_num
            if(dim_num*(j+1) > size(obj%FEMIface%NTS_NodCoord,2)  )then
                print *, "ContactMechanics_ >> GetActiveNTS >> dim_num(j+1) > size(obj%FEMIface%NTS_NodCoord,2)  "
                stop
			endif
            xm(j, 1:3)=obj%FEMIface%NTS_NodCoord(i,dim_num*j+1:dim_num*(j+1) ) ! ### Bug is here ### !
		enddo
		



		call GetNormalGap(xs,xm,gap)
		

    enddo

end subroutine
! #####################################################




! #####################################################
subroutine GetNormalGap(xs,xm,gap)
    real(real64),intent(in)::xs(:),xm(:,:)
    real(real64),intent(out)::gap

    real(real64),allocatable :: nm(:),am1(:),am2(:),mid(:),gvec(:)
    integer :: i,j,n,dim_num,ierr

    dim_num = size(xs)
    allocate(nm(3),am1(3),am2(3),mid(3),gvec(3) )

    if(dim_num == 1)then
        nm(:)=1.0d0
    elseif(dim_num == 2)then
        am1(1:2)=xm(1,1:2)
        am2(1:2)=0.0d0
        am2(3)=1.0d0
        nm(:)=cross_product(am1,am2)
        nm(:)=nm(:)/dsqrt(dot_product(nm,nm))
        gvec(:)=0.0d0
        gvec(:)=xs(:) - xm(1,:)
        gap=dot_product(gvec,nm)
    elseif(dim_num == 3)then
        
        am1(:)=xm(1,:)
        am2(:)=xm(2,:)
        nm(:)=cross_product(am1,am2)
        if(dot_product(nm,nm) == 0.0d0)then
            
            stop "df"            
        endif
        nm(:)=nm(:)/dsqrt(dot_product(nm,nm))
        gvec(:)=xs(:) - xm(1,:)
        gap=dot_product(gvec,nm)
    else
        print *, "Error >> GetNormalGap >> dim_num should be 1,2 or 3. "
        stop
    endif

end subroutine
! #####################################################


! #####################################################
subroutine getKcmat(obj,stick,StickSlip)
    class(ContactMechanics_),intent(inout)  :: obj
    logical,optional,intent(in)             :: Stick
    logical,optional,intent(in)             :: StickSlip

    if( present(stick) )then
        if(stick .eqv. .true.)then
            call obj%getKcmatStick()
        endif
    endif

    if( present(StickSlip) )then
        if(stick .eqv. .true.)then
            call obj%getKcmatStickSlip()
        endif
    endif

end subroutine
! #####################################################

! #####################################################
subroutine getKcmatStick(obj)
    class(ContactMechanics_),intent(inout)  :: obj


	real(real64),allocatable ::nts_amo(:,:),k_contact(:,:),fvec_contact(:)
	real(real64),allocatable :: old_nod_coord(:,:),uvec(:),contact_mat_para(:,:)
    integer             :: elem_id,nod_max
    integer,allocatable :: nts_elem_nod(:,:),active_nts(:),nts_mat(:)
    integer,allocatable :: stick_slip(:)
    
    integer             :: NumOfNode,NumOfNTSElem,i,nts_elem_id
     
    NumOfNTSElem=size(obj%FEMIface%NTS_ElemNod,1)
    
    NumOfNode=size(obj%FEMDomain1%Mesh%NodCoord,1)+size(obj%FEMDomain1%Mesh%NodCoord,2)
    
    do i=1,NumOfNTSElem
        nts_elem_id=i
        nod_max = NumOfNode

        call state_stick(nts_elem_id,nod_max,obj%GloNodCoord,obj%FEMIface%NTS_ElemNod,&
        obj%FEMIface%NTS_Active&
        ,obj%NTSVariables, obj%KcontactGlo,obj%NTSMaterial,&
        obj%ContactMatPara,obj%DispVecGlo,obj%FcontactGlo,obj%StickOrSlip)
    enddo
end subroutine
! #####################################################


! #####################################################
subroutine getKcmatStickSlip(obj)
    class(ContactMechanics_),intent(inout)  :: obj
    
end subroutine
! #####################################################


! #####################################################
! From here, imported from old library
! #####################################################
!=============================================================
!itr =1 ;state stick
!----------------------

 subroutine state_stick(j,nod_max,old_nod_coord,nts_elem_nod,active_nts&
     ,nts_amo, k_contact,nts_mat,contact_mat_para,uvec,fvec_contact,stick_slip)
      !現在のnts_elementについて、すべてstick状態としてk_contactの計算
    real(real64), allocatable ::x2s(:),x11(:),x12(:),evec(:),avec(:),nvec(:)&
	,k_st(:,:),ns(:),n0s(:),ts(:),ts_st(:),t0s(:),ngz0(:),fvec_e(:),nod_coord(:,:),&
	nvec_(:),tvec_(:),x1(:),x2(:),x3(:),x4(:),x5(:),x6(:),tvec(:),mvec(:),yi(:),Dns(:,:),&
	ym(:),ys(:),nvec__(:),ovec(:),mvec_(:),mvec__(:),Dns_1(:),Dns_2(:),Dns_3(:),domega_mat(:),&
	Dns_1_1(:),Ivec(:),dtmat(:,:),dmmat(:,:),dnmat__(:,:),dgzivec(:),dalpha(:),dHvec(:),nt(:),&
	Dnt(:,:),dT0vec(:),dtmat_(:,:),dselvec(:),dmmat_(:,:),dgzi_hat_vec(:),dganma_hat_vec(:),&
	dganmavec_(:),dnmat_(:,:),dgzivec_(:),dsjkvec(:),dlamdavec_(:),Svec(:),Ft(:),yL(:),tvec__(:),&
	ye(:),yj(:),yk(:),c_nod_coord(:,:)
	

	real(real64) ,intent(inout)::nts_amo(:,:),k_contact(:,:),fvec_contact(:)
	real(real64), intent(in) :: old_nod_coord(:,:),uvec(:),contact_mat_para(:,:)
    integer, intent(in) :: j, nod_max,nts_elem_nod(:,:),active_nts(:),nts_mat(:)
	integer, intent(inout) :: stick_slip(:)
    real(real64) c,phy,en,ct,gns,gz,l,pn,tts,gt,gz0,alpha,omega,gns_,gz_,sjk,delta
	real(real64) gzi_hat,delta_hat,ganma_,kappa,S0,ganma,gzi_,ganma_hat,lamda_,T0,dfdtn,HH,sel
	integer i, ii , k,beta,i_1,ii_1,node_ID
	 
	 ! 0 duvecの格納,ξ,ｇN等諸量の格納
	 allocate(x2s(2),x11(2),x12(2),evec(3),avec(3),nvec(3),k_st(8,8),&
	 ns(8),n0s(8),ts(8),t0s(8),ngz0(8),ts_st(8),fvec_e(8),tvec(2),mvec(2) )
	 allocate( nvec_(3),tvec_(3),x1(2),x2(2),x3(2),x4(2),x5(2),x6(2),yi(2) )
	 allocate(ym(2),ys(2),nvec__(2),ovec(2),mvec_(2),mvec__(2),Dns(8,8),Dns_1_1(8) )
	 allocate(Dns_1(8),domega_mat(8),Ivec(2) )
	allocate(dtmat(2,8),dmmat(2,8),dnmat__(2,8),dgzivec(8),dalpha(8),dHvec(8) )
	allocate(nod_coord(size(old_nod_coord,1),size(old_nod_coord,2)))
	allocate(nt(8),Dnt(8,8),dT0vec(8),dtmat_(2,8),dselvec(8),dmmat_(2,8),dgzi_hat_vec(8)  )
	allocate( dganma_hat_vec(8),dganmavec_(8),dnmat_(2,8),dgzivec_(8),dsjkvec(8),dlamdavec_(8)  )
	allocate(Svec(8),Ft(8),yL(2),tvec__(1:2) )
	
	allocate(ye(2),yj(2),yk(2),c_nod_coord(size(nod_coord,1),size(nod_coord,2)  )  )
	
	do i=1, size(nod_coord,1)
		nod_coord(i,1)=old_nod_coord(i,1)
		nod_coord(i,2)=old_nod_coord(i,2)
	enddo
	 do i=1,size(nod_coord,1)
		c_nod_coord(i,1)=nod_coord(i,1)+uvec(2*i-1)
		c_nod_coord(i,2)=nod_coord(i,2)+uvec(2*i  )
	 enddo
	 !-----材料パラメータの読み込み------
	 en=contact_mat_para(nts_mat( active_nts(j) ),2 )
	 ct=contact_mat_para(nts_mat( active_nts(j) ),1 )
	 c = contact_mat_para(nts_mat( active_nts(j) ),3 )
	 phy=contact_mat_para(nts_mat( active_nts(j) ),4 )
	 !--------------------------------
	 delta=1.0e-5
	 tts=nts_amo(active_nts(j),12)
	 nts_amo(active_nts(j),11)=tts 
	 !dfdtn=nts_amo(active_nts(j),10) 
	 if(tts>=0.0d0)then
		dfdtn=1.0d0
	  elseif(tts<0.0d0)then
		dfdtn=-1.0d0
	 else
		 stop "invalid tTs"
	  endif
	 !以下、初期座標＋変位により、位置ベクトルを更新し、諸量を更新
	 !gz更新
	 x1(1:2) = uvec(2*nts_elem_nod(active_nts(j),1)-1:&
	    2*nts_elem_nod(active_nts(j),1))+&
		nod_coord(nts_elem_nod(active_nts(j),1),1:2)
	 x2(1:2) = uvec(2*nts_elem_nod(active_nts(j),2)-1:&
	    2*nts_elem_nod(active_nts(j),2))+&
		nod_coord(nts_elem_nod(active_nts(j),2),1:2)
	 x3(1:2) = uvec(2*nts_elem_nod(active_nts(j),3)-1:&
	    2*nts_elem_nod(active_nts(j),3))+&
		nod_coord(nts_elem_nod(active_nts(j),3),1:2)	 
	 x4(1:2) = uvec(2*nts_elem_nod(active_nts(j),4)-1:&
	    2*nts_elem_nod(active_nts(j),4))+&
		nod_coord(nts_elem_nod(active_nts(j),4),1:2)
	x5(1:2) = uvec(2*nts_elem_nod(active_nts(j),5)-1:&
	    2*nts_elem_nod(active_nts(j),5))+&
		nod_coord(nts_elem_nod(active_nts(j),5),1:2)
	x6(1:2) = uvec(2*nts_elem_nod(active_nts(j),6)-1:&
	    2*nts_elem_nod(active_nts(j),6))+&
		nod_coord(nts_elem_nod(active_nts(j),6),1:2)
	 node_ID=active_nts(j)
	 
	 
	call get_beta_st_nts(node_ID,nts_elem_nod,c_nod_coord,beta)
	
	if(beta==1)then
		x2s(1:2) = x1(:)
		x11(1:2) = x2(:)
		x12(1:2) = x3(:)
		yi(1:2) = x4(:)
		yj(1:2) = x2(1:2)
		yk(1:2) = x3(1:2)
		ys(1:2) = x1(1:2)
		ym(1:2) = x2(1:2)
		ye(1:2) = x3(1:2)
		
		
		
	else
		x2s(1:2) = x1(:)
		x11(1:2) = x4(:)
		x12(1:2) = x2(:)
		yi(1:2) = x3(:)
		yj(1:2) = x4(1:2)
		yk(1:2) = x2(1:2)
		ys(1:2) = x1(1:2)
		ym(1:2) = x2(1:2)
		ye(1:2) = x4(1:2)
		
	endif
	
	 ! 0 duvecの格納,ξ,ｇN等諸量の格納
	 !-----------------------------------------------------------------------
	 
     !-----------------------------------------------------------------------

	 nvec(3) = 0.0d0
	 
	 avec(3) = 0.0d0
	 
	 evec(1) = 0.0d0
	 evec(2) = 0.0d0
	 evec(3) = 1.0d0
	 
	 Ivec(1) = 1.0d0
	 Ivec(2) = 1.0d0
	 
	 nvec_(3) = 0.0d0
	 tvec_(3) = 0.0d0
	!----------------------------------
	 l = dot_product( yj(1:2)-yk(1:2), yj(1:2)-yk(1:2)) 
	 l=dsqrt(l)
	sjk=l
	 if(l==0.0d0)then
		print *, "l=0 at element No.",node_ID
		 stop 
	 endif
	
	avec(1:2) = ( yk(1:2)-yj(1:2)  )/l

	 nvec(:) = cross_product(evec,avec)
	 gz=1.0d0/l*dot_product(ys(1:2)-yj(1:2),avec(1:2) )
	 gns = dot_product((ys(:)-ym(:)),nvec(1:2))
	 
	 

	 !alpha=4.0d0*gz*(1.0d0-gz)
	 !alpha=0.50d0*(1.0d0-cos(2.0d0*3.1415926535d0*gz) )
	 !alpha=exp( -delta*delta*(2.0d0*gz-1.0d0)**2.0d0 )
	 alpha=1.0d0
	 !alpha=0.0d0
	 yL(:)=yi(:)+alpha*(ym(:)-yi(:))
	 sel=dsqrt(dot_product(ye-yL,ye-yL))
	 gz0=gz-tts/ct/sel

	 if(sel==0.0d0)then
			 stop  "error check_gn"
	endif
	tvec_(1:2)=(ye(:)-yL(:) )/sel
	nvec_(:)=cross_product(evec,tvec_)
	tvec(1:2)=avec(1:2)
	mvec(:)=gz*tvec(:)-gns/sjk*nvec(:)
	nvec__(1:2)=nvec_(1:2)*dble(beta) 
	 
	 !gnsの計算と更新-----------------------------------------------------
	 gns_ = dot_product((ys(:)-ym(:)),nvec__(1:2))	 
	 gz_=1.0d0/sel*dot_product(ys-ym,tvec_(1:2) )
	 
	 !get f_contact(normal),K_contact(normal)
	 !compute common variables
	 gzi_=1.0d0/sel*dot_product(ys-ym,tvec_(1:2) )
	 ganma_hat=1.0d0/sel*dot_product(ym-yi,nvec_(1:2) )
	 !HH=4.0d0*(1.0d0-2.0d0*gz)
	 !HH=-3.1415926535d0*cos(2.0d0*3.1415926535d0*gz)
	 HH=alpha*(delta*delta)*(4.0d0-8.0d0*gz)
	 HH=0.0d0
	 
	 omega=1.0d0/sjk*HH*gz_*dot_product(ym-yi,nvec__(1:2) )
	 
	 gzi_hat=1.0d0/sel*dot_product(ym-yi,tvec_(1:2) )
	 delta_hat=dot_product(ym-yi,nvec_(1:2) )
	 ganma_=1.0d0/sel*dot_product(ys-ym,nvec_(1:2) )
	
	 ganma=gns/sjk
	 ovec(1:2)=gz*nvec(1:2)+ganma*tvec(1:2)
	 mvec_(1:2)=gzi_*tvec_(1:2)-ganma_*nvec_(1:2)
	 mvec__(1:2)=gzi_*tvec_(1:2)-ganma_*nvec_(1:2)
	 !kappa=-8.0d0
	 !kappa=-2.0d0*3.1415926535d0*3.1415926535d0*cos(2.0d0*3.1415926535d0*gz)
	 !kappa=8.0d0
	 kappa=alpha*(delta)*(delta)*(delta)*(delta)*(4.0d0-8.0d0*gz) - 8.0d0*alpha*(delta*delta)
	 kappa=0.0d0
	 tvec__(1:2)=dble(beta)*tvec_(1:2)
	 S0=delta_hat*dble(beta)/sjk*( kappa*gzi_+HH*HH*(2.0d0*gzi_*gzi_hat-ganma_*ganma_hat)  )
	 
	 if(beta==1)then
		!normal part >>>
		ns(1:2)=omega*(tvec(1:2)  )
		ns(3:4)=omega*(mvec(1:2)-tvec(1:2))!!+-
		ns(5:6)=omega*(-mvec(1:2)  )
		ns(7:8)=0.0d0
	 
		ns(1:2)=ns(1:2)+nvec__(1:2)
		ns(3:4)=ns(3:4)-(1.0d0-alpha*gz_)*nvec__(1:2) 
		ns(5:6)=ns(5:6)-gz_*nvec__(1:2)
		ns(7:8)=ns(7:8)+(1.0d0- alpha)*gz_*nvec__(1:2)
		
		Dns_1_1(1:2)=tvec(1:2)
		Dns_1_1(3:4)=mvec(1:2)-tvec(1:2)
		Dns_1_1(5:6)=-mvec(:)
		Dns_1_1(7:8)=0.0d0
		
		domega_mat(1:2)=1.0d0/sjk*S0*tvec(:)
		domega_mat(3:4)=1.0d0/sjk*(S0*(mvec(1:2)-tvec(1:2))+omega*tvec(1:2))!!+-
		domega_mat(5:6)=1.0d0/sjk*(-S0*mvec(1:2)-omega*tvec(1:2))
		domega_mat(7:8)=0.0d0
		
		domega_mat(1:2)=domega_mat(1:2)+HH/sjk*ganma_hat*tvec__(1:2)
		domega_mat(3:4)=domega_mat(3:4)+HH/sjk*( ganma_hat*(alpha*mvec__(1:2)-tvec__(1:2) )+gzi_*(1.0d0+alpha*gzi_hat)*nvec__(1:2))
		domega_mat(5:6)=domega_mat(5:6)+HH/sjk*(-ganma_hat*mvec__(1:2)-gzi_*ganma_hat*nvec__(1:2) ) 
		domega_mat(7:8)=domega_mat(7:8)+HH/sjk*(gzi_*(-1.0d0+(1.0d0-alpha )*gzi_hat)*nvec__(1:2)+ganma_hat*(1.0d0-alpha)*mvec__(1:2)  )
		
		dtmat(1:2,1:2)=0.0d0
		dtmat(1:2,3:4)=-diadic(nvec(1:2),nvec(1:2) )/sjk!!+-
		dtmat(1:2,5:6)=diadic(nvec(1:2),nvec(1:2) )/sjk
		dtmat(1:2,7:8)=0.0d0
		
		dmmat(1:2,1:2)=(diadic(tvec(1:2),tvec(1:2))-diadic(nvec(1:2),nvec(1:2) ))/sjk!!+-
		dmmat(1:2,3:4)=(-diadic(tvec(1:2),tvec(1:2))+diadic(nvec(1:2),nvec(1:2) )+diadic(tvec(1:2),mvec(1:2) )&
			-diadic(nvec(1:2),ovec(1:2))-diadic(ovec(1:2),nvec(1:2) ) )/sjk
		dmmat(1:2,5:6)=(-diadic(tvec(1:2),mvec(1:2) )&
			+diadic(nvec(1:2),ovec(1:2))+diadic(ovec(1:2),nvec(1:2) ) )/sjk
		dmmat(1:2,7:8)=0.0d0
		
		dnmat__(1:2,1:2)=HH*ganma_hat/sjk*diadic(tvec__(1:2),tvec(1:2) )
		dnmat__(1:2,3:4)=HH*ganma_hat/sjk*diadic(tvec__(1:2),mvec(1:2)-tvec(1:2) )!!+-
		dnmat__(1:2,5:6)=-HH*ganma_hat/sjk*diadic(tvec__(1:2),mvec(1:2))
		dnmat__(1:2,7:8)=0.0d0
		
		dnmat__(1:2,1:2)=dnmat__(1:2,1:2)+0.0d0
		dnmat__(1:2,3:4)=dnmat__(1:2,3:4)+1.0d0/sel*alpha*diadic(tvec__(1:2),nvec_(1:2) ) 
		dnmat__(1:2,5:6)=dnmat__(1:2,5:6)-1.0d0/sel*diadic(tvec__(1:2),nvec_(1:2) )
		dnmat__(1:2,7:8)=dnmat__(1:2,7:8)+1.0d0/sel*(1.0d0-alpha)*diadic(tvec__(1:2),nvec_(1:2) )
		 
		dgzivec(1:2)=1.0d0/sjk*tvec(1:2)
		dgzivec(3:4)=1.0d0/sjk*( mvec(1:2)-tvec(1:2) )!!+-
		dgzivec(5:6)=-1.0d0/sjk*mvec(1:2)
		dgzivec(7:8)=0.0d0
		
		dalpha(1:2)=HH/sjk*tvec(1:2)
		dalpha(3:4)=HH/sjk*( mvec(1:2)-tvec(1:2) )!!+-
		dalpha(5:6)=-HH/sjk*mvec(1:2)
		dalpha(7:8)=0.0d0
		
		dHvec(1:2)=kappa/sjk*tvec(1:2)
		dHvec(3:4)=kappa/sjk*( mvec(1:2)-tvec(1:2) )!!+-
		dHvec(5:6)=-kappa/sjk*mvec(1:2)
		dHvec(7:8)=0.0d0
		
		dgzivec_(1:2)=HH*(gzi_*gzi_hat-ganma_*ganma_hat)/sjk*tvec(1:2)
		dgzivec_(3:4)=HH*(gzi_*gzi_hat-ganma_*ganma_hat)/sjk*(mvec(1:2)-tvec(1:2) )!!+-
		dgzivec_(5:6)=-HH*(gzi_*gzi_hat-ganma_*ganma_hat)/sjk*mvec(1:2)
		dgzivec_(7:8)=0.0d0
		
		dgzivec_(1:2)=dgzivec_(1:2)+1.0d0/sel*tvec_(1:2)
		dgzivec_(3:4)=dgzivec_(3:4)+1.0d0/sel*(alpha*mvec_(1:2)-tvec_(1:2)) 
		dgzivec_(5:6)=dgzivec_(5:6)+1.0d0/sel*(-1.0d0)*mvec_(1:2)
		dgzivec_(7:8)=dgzivec_(7:8)+1.0d0/sel*(1.0d0-alpha)*mvec_(1:2)
		
		Dns(1:8,1:8)=diadic(Dns_1_1,domega_mat)
		
		
		Dns(1:2,1:8)=Dns(1:2,1:8)+omega*dtmat(1:2,1:8)
		Dns(3:4,1:8)=Dns(3:4,1:8)+omega*( dmmat(1:2,1:8)-dtmat(1:2,1:8) )!!+-
		Dns(5:6,1:8)=Dns(5:6,1:8)+omega*(-dmmat(1:2,1:8) )
		Dns(7:8,1:8)=Dns(7:8,1:8)+0.0d0
		
		Dns(1:2,1:8)=Dns(1:2,1:8)+dnmat__(1:2,1:8)
		Dns(3:4,1:8)=Dns(3:4,1:8)+diadic(nvec__(1:2),alpha*dgzivec_(1:8)+gzi_*dalpha(1:8) )-(1.0d0-alpha*gzi_)*dnmat__(1:2,1:8) 
		Dns(5:6,1:8)=Dns(5:6,1:8)-diadic(nvec__(1:2),dgzivec_(1:8) )-gzi_*dnmat__(1:2,1:8)
		Dns(7:8,1:8)=Dns(7:8,1:8)+diadic(nvec__(1:2),(1.0d0-alpha)*dgzivec_(1:8)-gzi_*dalpha(1:8) )+(1.0d0-alpha)*gzi_*dnmat__(1:2,1:8)
		
		
		fvec_e(1:8)= en*gns_*ns(1:8)
		K_st(1:8,1:8)=en*(diadic(ns,ns)+gns_*Dns(1:8,1:8) )
		! note >> du(1),du(2),du(3),du(4)
		
		!tangential part>>>
		
	
		dnmat_(1:2,1:2)=HH*ganma_hat/sjk*diadic(tvec_(1:2),tvec(1:2) )
		dnmat_(1:2,3:4)=HH*ganma_hat/sjk*diadic(tvec_(1:2),mvec(1:2)-tvec(1:2) )!!+-
		dnmat_(1:2,5:6)=-HH*ganma_hat/sjk*diadic(tvec_(1:2),mvec(1:2))
		dnmat_(1:2,7:8)=0.0d0
		
		dnmat_(1:2,1:2)=dnmat__(1:2,1:2)+0.0d0
		dnmat_(1:2,3:4)=dnmat__(1:2,3:4)+1.0d0/sel*alpha*diadic(tvec_(1:2),nvec_(1:2) ) 
		dnmat_(1:2,5:6)=dnmat__(1:2,5:6)-1.0d0/sel*diadic(tvec_(1:2),nvec_(1:2) )
		dnmat_(1:2,7:8)=dnmat__(1:2,7:8)+1.0d0/sel*(1.0d0-alpha)*diadic(tvec_(1:2),nvec_(1:2) )
		 
		
		dganmavec_(1:2)=HH*(gzi_*ganma_hat+gzi_hat*ganma_)/sjk*(tvec(1:2))
		dganmavec_(3:4)=HH*(gzi_*ganma_hat+gzi_hat*ganma_)/sjk*(mvec(1:2)-tvec(1:2))!!+-
		dganmavec_(5:6)=HH*(gzi_*ganma_hat+gzi_hat*ganma_)/sjk*(-mvec(1:2))
		dganmavec_(7:8)=0.0d0
		
		dganmavec_(1:2)=dganmavec_(1:2)+1.0d0/sel*(nvec_(1:2))
		dganmavec_(3:4)=dganmavec_(3:4)+1.0d0/sel*(alpha*(gzi_*nvec_(1:2)+ganma_*tvec_(1:2))-nvec_(1:2) ) 
		dganmavec_(5:6)=dganmavec_(5:6)+1.0d0/sel*(-(gzi_*nvec_(1:2)+ganma_*tvec_(1:2) ))
		dganmavec_(7:8)=dganmavec_(7:8)+1.0d0/sel*((1.0d0-alpha)*(gzi_*nvec_(1:2)+ganma_*tvec_(1:2)))
		
		dganma_hat_vec(1:2)=2.0d0*HH*gzi_hat*ganma_hat/sjk*(tvec(1:2))
		dganma_hat_vec(3:4)=2.0d0*HH*gzi_hat*ganma_hat/sjk*(mvec(1:2)-tvec(1:2))!!+-
		dganma_hat_vec(5:6)=2.0d0*HH*gzi_hat*ganma_hat/sjk*(-mvec(1:2))
		dganma_hat_vec(7:8)=0.0d0
		
		
		dganma_hat_vec(1:2)=dganma_hat_vec(1:2)+0.0d0
		dganma_hat_vec(3:4)=dganma_hat_vec(3:4)+1.0d0/sel*(alpha*(gzi_hat*nvec_(1:2)+ganma_hat*tvec_(1:2))+nvec_(1:2) ) 
		dganma_hat_vec(5:6)=dganma_hat_vec(5:6)+1.0d0/sel*(-(gzi_hat*nvec_(1:2)+ganma_hat*tvec_(1:2) ))
		dganma_hat_vec(7:8)=dganma_hat_vec(7:8)+1.0d0/sel*((1.0d0-alpha)*(gzi_hat*nvec_(1:2)+ganma_hat*tvec_(1:2))-nvec_(1:2) )
		
		dgzi_hat_vec(1:2)=HH*(gzi_hat*gzi_hat-ganma_hat*ganma_hat)/sjk*(tvec(1:2))
		dgzi_hat_vec(3:4)=HH*(gzi_hat*gzi_hat-ganma_hat*ganma_hat)/sjk*(mvec(1:2)-tvec(1:2))!!+-
		dgzi_hat_vec(5:6)=HH*(gzi_hat*gzi_hat-ganma_hat*ganma_hat)/sjk*(-mvec(1:2))
		dgzi_hat_vec(7:8)=0.0d0
		
		dgzi_hat_vec(1:2)=dgzi_hat_vec(1:2)+0.0d0
		dgzi_hat_vec(3:4)=dgzi_hat_vec(3:4)+1.0d0/sel*((gzi_hat*tvec_(1:2)-ganma_hat*nvec_(1:2))*alpha+tvec_(1:2) )
		dgzi_hat_vec(5:6)=dgzi_hat_vec(5:6)+1.0d0/sel*(-(gzi_hat*tvec_(1:2)-ganma_hat*nvec_(1:2)) ) 
		dgzi_hat_vec(7:8)=dgzi_hat_vec(7:8)+1.0d0/sel*((1.0d0-alpha)*(gzi_hat*tvec_(1:2)-ganma_hat*nvec_(1:2))-tvec_(1:2) )
		
		
		
		
		dtmat_(1:2,1:2)=HH*ganma_hat/sjk*(-1.0d0)*(diadic( nvec_(1:2),tvec(1:2) ) )
		dtmat_(1:2,3:4)=HH*ganma_hat/sjk*(-1.0d0)*(diadic( nvec_(1:2),mvec(1:2)-tvec(1:2) ) )!!+-
		dtmat_(1:2,5:6)=HH*ganma_hat/sjk*(1.0d0)*(diadic( nvec_(1:2),mvec(1:2) ) )
		dtmat_(1:2,7:8)=0.0d0
	
		dtmat_(1:2,1:2)=dtmat_(1:2,1:2)+0.0d0
		dtmat_(1:2,3:4)=dtmat_(1:2,3:4)+1.0d0/sel*(-1.0d0)*alpha*diadic( nvec_(1:2), nvec_(1:2) ) 
		dtmat_(1:2,5:6)=dtmat_(1:2,5:6)+1.0d0/sel*(1.0d0)*diadic( nvec_(1:2), nvec_(1:2) )
		dtmat_(1:2,7:8)=dtmat_(1:2,7:8)+1.0d0/sel*(-1.0d0+alpha)*diadic( nvec_(1:2), nvec_(1:2) )
		
		
		
		
		dmmat_(1:2,1:8)=diadic(tvec_(1:2),dgzivec_(1:8) )
		
		dmmat_(1:2,1:8)=dmmat_(1:2,1:8)+gzi_*dtmat_(1:2,1:8)
		
		dmmat_(1:2,1:8)=dmmat_(1:2,1:8)+diadic( nvec_(1:2), dganmavec_(1:8))
		
		dmmat_(1:2,1:8)=dmmat_(1:2,1:8)+ganma_*dnmat_(1:2,1:8)
		
		dselvec(1:2)=sel*HH*gzi_hat/sjk*(-1.0d0)*tvec(1:2)
		dselvec(3:4)=sel*HH*gzi_hat/sjk*(-1.0d0)*(mvec(1:2)-tvec(1:2))
		dselvec(5:6)=sel*HH*gzi_hat/sjk*mvec(1:2)
		dselvec(7:8)=0.0d0
		
		dselvec(1:2)=dselvec(1:2)+0.0d0
		dselvec(3:4)=dselvec(3:4)+(-alpha)*tvec_(1:2) !!+-
		dselvec(5:6)=dselvec(5:6)+tvec_(1:2)
		dselvec(7:8)=dselvec(7:8)-(1.0d0-alpha)*tvec_(1:2)
		
		dlamdavec_(1:8)=gzi_*dgzi_hat_vec(1:8)+gzi_hat*dgzivec_(1:8)&
			-ganma_hat*dganmavec_(1:8)-ganma_*dganma_hat_vec(1:8)
		
		!original part
		dsjkvec(1:2)=dble(beta)*0.0d0
		dsjkvec(3:4)=dble(beta)*(-1.0d0)*tvec(1:2)
		dsjkvec(5:6)=dble(beta)*tvec(1:2)
		dsjkvec(7:8)=dble(beta)*0.0d0
		
		lamda_=gzi_*gzi_hat-ganma_*ganma_hat
		T0=1.0d0/sjk*HH*lamda_
		
		dT0vec(1:8)=-HH*lamda_/sjk/sjk*dsjkvec(1:8)+HH/sjk*dlamdavec_(1:8)+lamda_/sjk*dHvec(1:8)
		
		Svec(1:2)=-sel*HH*gzi_hat/sjk*tvec(1:2)
		Svec(3:4)=-sel*HH*gzi_hat/sjk*mvec(1:2)-tvec(1:2)!!+-
		Svec(5:6)=-sel*HH*gzi_hat/sjk*(-mvec(1:2))
		Svec(7:8)=0.0d0
		
		Svec(1:2)=Svec(1:2)+0.0d0
		Svec(3:4)=Svec(3:4)+(-alpha)*tvec_(1:2)  
		Svec(5:6)=Svec(5:6)+tvec_(1:2)
		Svec(7:8)=Svec(7:8)-(1.0d0-alpha)*tvec_(1:2)
		
		nt(1:2)=T0*tvec(1:2)
		nt(3:4)=T0*( mvec(1:2)-tvec(1:2)     )!!+-
		nt(5:6)=T0*(-mvec(1:2) )
		nt(7:8)=0.0d0		
			
		nt(1:2)=nt(1:2)+1.0d0/sel*tvec_(1:2)
		nt(3:4)=nt(3:4)+1.0d0/sel*( alpha*mvec_(1:2)-tvec_(1:2)  ) 
		nt(5:6)=nt(5:6)+1.0d0/sel*(-mvec_(1:2))
		nt(7:8)=nt(7:8)+1.0d0/sel*(1.0d0-alpha )*mvec_(1:2)		

		
		Dnt(1:2,1:8)=diadic(tvec(1:2),dT0vec(1:8) )+T0*dtmat(1:2,1:8)
		Dnt(3:4,1:8)=diadic(mvec(1:2)-tvec(1:2),dT0vec(1:8) )+T0*(dmmat(1:2,1:8)  -dtmat(1:2,1:8))!!+-
		Dnt(5:6,1:8)=-diadic(mvec(1:2),dT0vec(1:8) )-T0*dmmat(1:2,1:8)
		Dnt(7:8,1:8)=0.0d0
		
		Dnt(1:2,1:8)=Dnt(1:2,1:8)-1.0d0/sel/sel*diadic(tvec_(1:2),dselvec(1:8) )
		Dnt(3:4,1:8)=Dnt(3:4,1:8)-1.0d0/sel/sel*diadic( alpha*mvec_(1:2)- tvec_(1:2),dselvec(1:8) ) !inverse original
		Dnt(5:6,1:8)=Dnt(5:6,1:8)-1.0d0/sel/sel*diadic(-mvec_(1:2),dselvec(1:8) )
		Dnt(7:8,1:8)=Dnt(7:8,1:8)-1.0d0/sel/sel*diadic( (1.0d0-alpha)*mvec_(1:2),dselvec(1:8) )
		
		Dnt(1:2,1:8)=Dnt(1:2,1:8)+1.0d0/sel*dtmat_(1:2,1:8)
		Dnt(3:4,1:8)=Dnt(3:4,1:8)+1.0d0/sel*(diadic(mvec_(1:2),dalpha(1:8) )+alpha*dmmat_(1:2,1:8)-dtmat_(1:2,1:8))!inverse original
		Dnt(5:6,1:8)=Dnt(5:6,1:8)+1.0d0/sel*(-dmmat_(1:2,1:8) )
		Dnt(7:8,1:8)=Dnt(7:8,1:8)+1.0d0/sel*(-diadic(mvec_(1:2),dalpha(1:8) )+(1.0d0-alpha)*dmmat_(1:2,1:8))
		
		
		if(stick_slip( active_nts(j)  )==0  )then
			Ft(1:8)=dble(beta)*ct*sel*nt(1:8)
		elseif(stick_slip( active_nts(j)  )==1  )then
			Ft(1:8)=en*tan(phy)*ns(1:8)
		else
			 stop  "invalid stick_slip on contact.f95"
		endif
		fvec_e(1:8)= fvec_e(1:8)+dble(beta)*tts*sel*nt(1:8) 
		K_st(1:8,1:8)=K_st(1:8,1:8)+dble(beta)*transpose( sel*diadic(Ft(1:8),nt(1:8))+tts*diadic(Svec(1:8),nt(1:8))+tts*sel*Dnt(1:8,1:8))
		fvec_e(:)=fvec_e(:)*l !integration
		K_st(:,:)=K_st(:,:)*l !integration
		do i = 1,4
			do ii = 1, 4
			
			k_contact(2*nts_elem_nod(active_nts(j),i)-1,2*nts_elem_nod(active_nts(j),ii)-1) &
			=k_contact(2*nts_elem_nod(active_nts(j),i)-1,2*nts_elem_nod(active_nts(j),ii)-1) &
			+k_st(2*i-1,2*ii-1)
			k_contact(2*nts_elem_nod(active_nts(j),i)-1,2*nts_elem_nod(active_nts(j),ii)) &
			=k_contact(2*nts_elem_nod(active_nts(j),i)-1,2*nts_elem_nod(active_nts(j),ii)) &
			+k_st(2*i-1,2*ii)
			k_contact(2*nts_elem_nod(active_nts(j),i),2*nts_elem_nod(active_nts(j),ii)-1) &
			=k_contact(2*nts_elem_nod(active_nts(j),i),2*nts_elem_nod(active_nts(j),ii)-1) &
			+k_st(2*i,2*ii-1)
			k_contact(2*nts_elem_nod(active_nts(j),i),2*nts_elem_nod(active_nts(j),ii)) &
			=k_contact(2*nts_elem_nod(active_nts(j),i),2*nts_elem_nod(active_nts(j),ii)) &
			+k_st(2*i,2*ii)
		
			enddo
		enddo
		
		do i=1,4
			fvec_contact(2*nts_elem_nod(active_nts(j),i)-1 ) &
				=fvec_contact(2*nts_elem_nod(active_nts(j),i)-1 )+fvec_e(2*i-1)
			fvec_contact(2*nts_elem_nod(active_nts(j),i)) &
				=fvec_contact(2*nts_elem_nod(active_nts(j),i))+fvec_e(2*i)	
		enddo
	

	

		
	 elseif(beta==-1)then
		!normal part >>>
		!normal part >>>
		ns(1:2)=omega*(tvec(1:2)  )
		ns(3:4)=omega*(-mvec(1:2)  )
		ns(5:6)=omega*(mvec(1:2)-tvec(1:2))!!+-
		ns(7:8)=0.0d0
	 
		ns(1:2)=ns(1:2)+nvec__(1:2)
		ns(3:4)=ns(3:4)-(1.0d0-alpha*gz_)*nvec__(1:2) 
		ns(5:6)=ns(5:6)-gz_*nvec__(1:2)
		ns(7:8)=ns(7:8)+(1.0d0- alpha)*gz_*nvec__(1:2)
		
		Dns_1_1(1:2)=tvec(1:2)
		Dns_1_1(3:4)=-mvec(:)
		Dns_1_1(5:6)=mvec(1:2)-tvec(1:2)!!+-
		Dns_1_1(7:8)=0.0d0
		
		domega_mat(1:2)=1.0d0/sjk*S0*tvec(:)
		domega_mat(3:4)=1.0d0/sjk*(-S0*mvec(1:2)-omega*tvec(1:2))
		domega_mat(5:6)=1.0d0/sjk*(S0*(mvec(1:2)-tvec(1:2))+omega*tvec(1:2))!!+-
		domega_mat(7:8)=0.0d0
		
		domega_mat(1:2)=domega_mat(1:2)+HH/sjk*ganma_hat*tvec__(1:2)
		domega_mat(3:4)=domega_mat(3:4)+HH/sjk*( ganma_hat*(alpha*mvec__(1:2)-tvec__(1:2) )+gzi_*(1.0d0+alpha*gzi_hat)*nvec__(1:2))
		domega_mat(5:6)=domega_mat(5:6)+HH/sjk*(-ganma_hat*mvec__(1:2)-gzi_*ganma_hat*nvec__(1:2) ) 
		domega_mat(7:8)=domega_mat(7:8)+HH/sjk*(gzi_*(-1.0d0+(1.0d0-alpha )*gzi_hat)*nvec__(1:2)+ganma_hat*(1.0d0-alpha)*mvec__(1:2)  )
		
		dtmat(1:2,1:2)=0.0d0
		dtmat(1:2,3:4)=diadic(nvec(1:2),nvec(1:2) )/sjk
		dtmat(1:2,5:6)=-diadic(nvec(1:2),nvec(1:2) )/sjk!!+-
		dtmat(1:2,7:8)=0.0d0
		
		dmmat(1:2,1:2)=(diadic(tvec(1:2),tvec(1:2))-diadic(nvec(1:2),nvec(1:2) ))/sjk
		dmmat(1:2,3:4)=(-diadic(tvec(1:2),mvec(1:2) )&
			+diadic(nvec(1:2),ovec(1:2))+diadic(ovec(1:2),nvec(1:2) ) )/sjk
		dmmat(1:2,5:6)=(-diadic(tvec(1:2),tvec(1:2))+diadic(nvec(1:2),nvec(1:2) )+diadic(tvec(1:2),mvec(1:2) )&
			-diadic(nvec(1:2),ovec(1:2))-diadic(ovec(1:2),nvec(1:2) ) )/sjk!!+-
		dmmat(1:2,7:8)=0.0d0
		
		dnmat__(1:2,1:2)=HH*ganma_hat/sjk*diadic(tvec__(1:2),tvec(1:2) )
		dnmat__(1:2,3:4)=-HH*ganma_hat/sjk*diadic(tvec__(1:2),mvec(1:2))
		dnmat__(1:2,5:6)=HH*ganma_hat/sjk*diadic(tvec__(1:2),mvec(1:2)-tvec(1:2) )!!+-
		dnmat__(1:2,7:8)=0.0d0
		
		dnmat__(1:2,1:2)=dnmat__(1:2,1:2)+0.0d0
		dnmat__(1:2,3:4)=dnmat__(1:2,3:4)+1.0d0/sel*alpha*diadic(tvec__(1:2),nvec_(1:2) ) 
		dnmat__(1:2,5:6)=dnmat__(1:2,5:6)-1.0d0/sel*diadic(tvec__(1:2),nvec_(1:2) )
		dnmat__(1:2,7:8)=dnmat__(1:2,7:8)+1.0d0/sel*(1.0d0-alpha)*diadic(tvec__(1:2),nvec_(1:2) )
		 
		dgzivec(1:2)=1.0d0/sjk*tvec(1:2)
		dgzivec(3:4)=-1.0d0/sjk*mvec(1:2)
		dgzivec(5:6)=1.0d0/sjk*( mvec(1:2)-tvec(1:2) )!!+-
		dgzivec(7:8)=0.0d0
		
		dalpha(1:2)=HH/sjk*tvec(1:2)
		dalpha(3:4)=-HH/sjk*mvec(1:2)
		dalpha(5:6)=HH/sjk*( mvec(1:2)-tvec(1:2) )!!+-
		dalpha(7:8)=0.0d0
		
		dHvec(1:2)=kappa/sjk*tvec(1:2)
		dHvec(3:4)=-kappa/sjk*mvec(1:2)
		dHvec(5:6)=kappa/sjk*( mvec(1:2)-tvec(1:2) )!!+-
		dHvec(7:8)=0.0d0
		
		dgzivec_(1:2)=HH*(gzi_*gzi_hat-ganma_*ganma_hat)/sjk*tvec(1:2)
		dgzivec_(3:4)=-HH*(gzi_*gzi_hat-ganma_*ganma_hat)/sjk*mvec(1:2)
		dgzivec_(5:6)=HH*(gzi_*gzi_hat-ganma_*ganma_hat)/sjk*(mvec(1:2)-tvec(1:2) )!!+-
		dgzivec_(7:8)=0.0d0
		
		dgzivec_(1:2)=dgzivec_(1:2)+1.0d0/sel*tvec_(1:2)
		dgzivec_(3:4)=dgzivec_(3:4)+1.0d0/sel*(alpha*mvec_(1:2)-tvec_(1:2)) 
		dgzivec_(5:6)=dgzivec_(5:6)+1.0d0/sel*(-1.0d0)*mvec_(1:2)
		dgzivec_(7:8)=dgzivec_(7:8)+1.0d0/sel*(1.0d0-alpha)*mvec_(1:2)
		
		Dns(1:8,1:8)=diadic(Dns_1_1,domega_mat)
		
		
		Dns(1:2,1:8)=Dns(1:2,1:8)+omega*dtmat(1:2,1:8)
		Dns(3:4,1:8)=Dns(3:4,1:8)+omega*(-dmmat(1:2,1:8) )
		Dns(5:6,1:8)=Dns(5:6,1:8)+omega*( dmmat(1:2,1:8)-dtmat(1:2,1:8) )!!+-
		Dns(7:8,1:8)=Dns(7:8,1:8)+0.0d0
		
		Dns(1:2,1:8)=Dns(1:2,1:8)+dnmat__(1:2,1:8)
		Dns(3:4,1:8)=Dns(3:4,1:8)+diadic(nvec__(1:2),alpha*dgzivec_(1:8)+gzi_*dalpha(1:8) )-(1.0d0-alpha*gzi_)*dnmat__(1:2,1:8) 
		Dns(5:6,1:8)=Dns(5:6,1:8)-diadic(nvec__(1:2),dgzivec_(1:8) )-gzi_*dnmat__(1:2,1:8)
		Dns(7:8,1:8)=Dns(7:8,1:8)+diadic(nvec__(1:2),(1.0d0-alpha)*dgzivec_(1:8)-gzi_*dalpha(1:8) )+(1.0d0-alpha)*gzi_*dnmat__(1:2,1:8)
		
		
		fvec_e(1:8)= en*gns_*ns(1:8)
		K_st(1:8,1:8)=en*(diadic(ns,ns)+gns_*Dns(1:8,1:8) )
		! note >> du(1),du(2),du(3),du(4)
		
		!tangential part>>>
		
	
		dnmat_(1:2,1:2)=HH*ganma_hat/sjk*diadic(tvec_(1:2),tvec(1:2) )
		dnmat_(1:2,3:4)=-HH*ganma_hat/sjk*diadic(tvec_(1:2),mvec(1:2))
		dnmat_(1:2,5:6)=HH*ganma_hat/sjk*diadic(tvec_(1:2),mvec(1:2)-tvec(1:2) )!!+-
		dnmat_(1:2,7:8)=0.0d0
		
		dnmat_(1:2,1:2)=dnmat__(1:2,1:2)+0.0d0
		dnmat_(1:2,3:4)=dnmat__(1:2,3:4)+1.0d0/sel*alpha*diadic(tvec_(1:2),nvec_(1:2) ) 
		dnmat_(1:2,5:6)=dnmat__(1:2,5:6)-1.0d0/sel*diadic(tvec_(1:2),nvec_(1:2) )
		dnmat_(1:2,7:8)=dnmat__(1:2,7:8)+1.0d0/sel*(1.0d0-alpha)*diadic(tvec_(1:2),nvec_(1:2) )
		 
		
		dganmavec_(1:2)=HH*(gzi_*ganma_hat+gzi_hat*ganma_)/sjk*(tvec(1:2))
		dganmavec_(3:4)=HH*(gzi_*ganma_hat+gzi_hat*ganma_)/sjk*(-mvec(1:2))
		dganmavec_(5:6)=HH*(gzi_*ganma_hat+gzi_hat*ganma_)/sjk*(mvec(1:2)-tvec(1:2))!!+-
		dganmavec_(7:8)=0.0d0
		
		dganmavec_(1:2)=dganmavec_(1:2)+1.0d0/sel*(nvec_(1:2))
		dganmavec_(3:4)=dganmavec_(3:4)+1.0d0/sel*(alpha*(gzi_*nvec_(1:2)+ganma_*tvec_(1:2))-nvec_(1:2) ) 
		dganmavec_(5:6)=dganmavec_(5:6)+1.0d0/sel*(-(gzi_*nvec_(1:2)+ganma_*tvec_(1:2) ))
		dganmavec_(7:8)=dganmavec_(7:8)+1.0d0/sel*((1.0d0-alpha)*(gzi_*nvec_(1:2)+ganma_*tvec_(1:2)))
		
		dganma_hat_vec(1:2)=2.0d0*HH*gzi_hat*ganma_hat/sjk*(tvec(1:2))
		dganma_hat_vec(3:4)=2.0d0*HH*gzi_hat*ganma_hat/sjk*(-mvec(1:2))
		dganma_hat_vec(5:6)=2.0d0*HH*gzi_hat*ganma_hat/sjk*(mvec(1:2)-tvec(1:2))!!+-
		dganma_hat_vec(7:8)=0.0d0
		
		
		dganma_hat_vec(1:2)=dganma_hat_vec(1:2)+0.0d0
		dganma_hat_vec(3:4)=dganma_hat_vec(3:4)+1.0d0/sel*(alpha*(gzi_hat*nvec_(1:2)+ganma_hat*tvec_(1:2))+nvec_(1:2) ) 
		dganma_hat_vec(5:6)=dganma_hat_vec(5:6)+1.0d0/sel*(-(gzi_hat*nvec_(1:2)+ganma_hat*tvec_(1:2) ))
		dganma_hat_vec(7:8)=dganma_hat_vec(7:8)+1.0d0/sel*((1.0d0-alpha)*(gzi_hat*nvec_(1:2)+ganma_hat*tvec_(1:2))-nvec_(1:2) )
		
		dgzi_hat_vec(1:2)=HH*(gzi_hat*gzi_hat-ganma_hat*ganma_hat)/sjk*(tvec(1:2))
		dgzi_hat_vec(3:4)=HH*(gzi_hat*gzi_hat-ganma_hat*ganma_hat)/sjk*(-mvec(1:2))
		dgzi_hat_vec(5:6)=HH*(gzi_hat*gzi_hat-ganma_hat*ganma_hat)/sjk*(mvec(1:2)-tvec(1:2))!!+-
		dgzi_hat_vec(7:8)=0.0d0
		
		dgzi_hat_vec(1:2)=dgzi_hat_vec(1:2)+0.0d0
		dgzi_hat_vec(3:4)=dgzi_hat_vec(3:4)+1.0d0/sel*((gzi_hat*tvec_(1:2)-ganma_hat*nvec_(1:2))*alpha+tvec_(1:2) )
		dgzi_hat_vec(5:6)=dgzi_hat_vec(5:6)+1.0d0/sel*(-(gzi_hat*tvec_(1:2)-ganma_hat*nvec_(1:2)) ) 
		dgzi_hat_vec(7:8)=dgzi_hat_vec(7:8)+1.0d0/sel*((1.0d0-alpha)*(gzi_hat*tvec_(1:2)-ganma_hat*nvec_(1:2))-tvec_(1:2) )
		
		
		
		
		dtmat_(1:2,1:2)=HH*ganma_hat/sjk*(-1.0d0)*(diadic( nvec_(1:2),tvec(1:2) ) )
		dtmat_(1:2,3:4)=HH*ganma_hat/sjk*(1.0d0)*(diadic( nvec_(1:2),mvec(1:2) ) )
		dtmat_(1:2,5:6)=HH*ganma_hat/sjk*(-1.0d0)*(diadic( nvec_(1:2),mvec(1:2)-tvec(1:2) ) )!!+-
		dtmat_(1:2,7:8)=0.0d0
	
		dtmat_(1:2,1:2)=dtmat_(1:2,1:2)+0.0d0
		dtmat_(1:2,3:4)=dtmat_(1:2,3:4)+1.0d0/sel*(-1.0d0)*alpha*diadic( nvec_(1:2), nvec_(1:2) ) 
		dtmat_(1:2,5:6)=dtmat_(1:2,5:6)+1.0d0/sel*(1.0d0)*diadic( nvec_(1:2), nvec_(1:2) )
		dtmat_(1:2,7:8)=dtmat_(1:2,7:8)+1.0d0/sel*(-1.0d0+alpha)*diadic( nvec_(1:2), nvec_(1:2) )
		
		
		
		
		dmmat_(1:2,1:8)=diadic(tvec_(1:2),dgzivec_(1:8) )
		
		dmmat_(1:2,1:8)=dmmat_(1:2,1:8)+gzi_*dtmat_(1:2,1:8)
		
		dmmat_(1:2,1:8)=dmmat_(1:2,1:8)+diadic( nvec_(1:2), dganmavec_(1:8))
		
		dmmat_(1:2,1:8)=dmmat_(1:2,1:8)+ganma_*dnmat_(1:2,1:8)
		
		dselvec(1:2)=sel*HH*gzi_hat/sjk*(-1.0d0)*tvec(1:2)
		dselvec(3:4)=sel*HH*gzi_hat/sjk*(-1.0d0)*(mvec(1:2)-tvec(1:2))
		dselvec(5:6)=sel*HH*gzi_hat/sjk*mvec(1:2)
		dselvec(7:8)=0.0d0
		
		dselvec(1:2)=dselvec(1:2)+0.0d0
		dselvec(3:4)=dselvec(3:4)+tvec_(1:2)
		dselvec(5:6)=dselvec(5:6)+(-alpha)*tvec_(1:2) !!+-
		dselvec(7:8)=dselvec(7:8)-(1.0d0-alpha)*tvec_(1:2)
		
		dlamdavec_(1:8)=gzi_*dgzi_hat_vec(1:8)+gzi_hat*dgzivec_(1:8)&
			-ganma_hat*dganmavec_(1:8)-ganma_*dganma_hat_vec(1:8)
		
		!original part
		dsjkvec(1:2)=dble(beta)*0.0d0
		dsjkvec(3:4)=dble(beta)*(-1.0d0)*tvec(1:2)
		dsjkvec(5:6)=dble(beta)*tvec(1:2)
		dsjkvec(7:8)=dble(beta)*0.0d0
		
		lamda_=gzi_*gzi_hat-ganma_*ganma_hat
		T0=1.0d0/sjk*HH*lamda_
		
		dT0vec(1:8)=-HH*lamda_/sjk/sjk*dsjkvec(1:8)+HH/sjk*dlamdavec_(1:8)+lamda_/sjk*dHvec(1:8)
		
		Svec(1:2)=-sel*HH*gzi_hat/sjk*tvec(1:2)
		Svec(3:4)=-sel*HH*gzi_hat/sjk*(-mvec(1:2))
		Svec(5:6)=-sel*HH*gzi_hat/sjk*mvec(1:2)-tvec(1:2)!!+-
		Svec(7:8)=0.0d0
		
		Svec(1:2)=Svec(1:2)+0.0d0
		Svec(3:4)=Svec(3:4)+(-alpha)*tvec_(1:2)  
		Svec(5:6)=Svec(5:6)+tvec_(1:2)
		Svec(7:8)=Svec(7:8)-(1.0d0-alpha)*tvec_(1:2)
		
		nt(1:2)=T0*tvec(1:2)
		nt(3:4)=T0*(-mvec(1:2) )
		nt(5:6)=T0*( mvec(1:2)-tvec(1:2)     )!!+-
		nt(7:8)=0.0d0		
			
		nt(1:2)=nt(1:2)+1.0d0/sel*tvec_(1:2)
		nt(3:4)=nt(3:4)+1.0d0/sel*( alpha*mvec_(1:2)-tvec_(1:2)  ) 
		nt(5:6)=nt(5:6)+1.0d0/sel*(-mvec_(1:2))
		nt(7:8)=nt(7:8)+1.0d0/sel*(1.0d0-alpha )*mvec_(1:2)		

		
		Dnt(1:2,1:8)=diadic(tvec(1:2),dT0vec(1:8) )+T0*dtmat(1:2,1:8)
		Dnt(3:4,1:8)=-diadic(mvec(1:2),dT0vec(1:8) )-T0*dmmat(1:2,1:8)
		Dnt(5:6,1:8)=diadic(mvec(1:2)-tvec(1:2),dT0vec(1:8) )+T0*(dmmat(1:2,1:8)  -dtmat(1:2,1:8))!!+-
		Dnt(7:8,1:8)=0.0d0
		
		Dnt(1:2,1:8)=Dnt(1:2,1:8)-1.0d0/sel/sel*diadic(tvec_(1:2),dselvec(1:8) )
		Dnt(3:4,1:8)=Dnt(3:4,1:8)-1.0d0/sel/sel*diadic( alpha*mvec_(1:2)- tvec_(1:2),dselvec(1:8) ) !inverse original
		Dnt(5:6,1:8)=Dnt(5:6,1:8)-1.0d0/sel/sel*diadic(-mvec_(1:2),dselvec(1:8) )
		Dnt(7:8,1:8)=Dnt(7:8,1:8)-1.0d0/sel/sel*diadic( (1.0d0-alpha)*mvec_(1:2),dselvec(1:8) )
		
		Dnt(1:2,1:8)=Dnt(1:2,1:8)+1.0d0/sel*dtmat_(1:2,1:8)
		Dnt(3:4,1:8)=Dnt(3:4,1:8)+1.0d0/sel*(diadic(mvec_(1:2),dalpha(1:8) )+alpha*dmmat_(1:2,1:8)-dtmat_(1:2,1:8))!inverse original
		Dnt(5:6,1:8)=Dnt(5:6,1:8)+1.0d0/sel*(-dmmat_(1:2,1:8) )
		Dnt(7:8,1:8)=Dnt(7:8,1:8)+1.0d0/sel*(-diadic(mvec_(1:2),dalpha(1:8) )+(1.0d0-alpha)*dmmat_(1:2,1:8))
		
		
		if(stick_slip( active_nts(j)  )==0  )then
			Ft(1:8)=dble(beta)*ct*sel*nt(1:8)
		elseif(stick_slip( active_nts(j)  )==1  )then
			Ft(1:8)=en*tan(phy)*ns(1:8)
		else
			 stop  "invalid stick_slip on contact.f95"
		endif
		fvec_e(1:8)= fvec_e(1:8)+dble(beta)*tts*sel*nt(1:8)
		K_st(1:8,1:8)=K_st(1:8,1:8)+dble(beta)*transpose( sel*diadic(Ft(1:8),nt(1:8))+tts*diadic(Svec(1:8),nt(1:8))+tts*sel*Dnt(1:8,1:8))
		
		fvec_e(:)=fvec_e(:)*l !integration
		K_st(:,:)=K_st(:,:)*l !integration
		do i = 1,4
			do ii = 1, 4
				if(i==3)then
					i_1=4
				elseif(i==4)then
					i_1=3
				else
					i_1=i
				endif
				
				if(ii==3)then
					ii_1=4
				elseif(ii==4)then
					ii_1=3
				else
					ii_1=ii
				endif
				
				k_contact(2*nts_elem_nod(active_nts(j),i_1)-1,2*nts_elem_nod(active_nts(j),ii_1)-1) &
				=k_contact(2*nts_elem_nod(active_nts(j),i_1)-1,2*nts_elem_nod(active_nts(j),ii_1)-1) &
				+k_st(2*i_1-1,2*ii_1-1)
				k_contact(2*nts_elem_nod(active_nts(j),i_1)-1,2*nts_elem_nod(active_nts(j),ii_1)) &
				=k_contact(2*nts_elem_nod(active_nts(j),i_1)-1,2*nts_elem_nod(active_nts(j),ii_1)) &
				+k_st(2*i_1-1,2*ii_1)
				k_contact(2*nts_elem_nod(active_nts(j),i_1),2*nts_elem_nod(active_nts(j),ii_1)-1) &
				=k_contact(2*nts_elem_nod(active_nts(j),i_1),2*nts_elem_nod(active_nts(j),ii_1)-1) &
				+k_st(2*i_1,2*ii_1-1)
				k_contact(2*nts_elem_nod(active_nts(j),i_1),2*nts_elem_nod(active_nts(j),ii_1)) &
				=k_contact(2*nts_elem_nod(active_nts(j),i_1),2*nts_elem_nod(active_nts(j),ii_1)) &
				+k_st(2*i_1,2*ii_1)
			
			enddo
		enddo
		
		do i=1,4
			if(i==3)then
				i_1=4
			elseif(i==4)then
				i_1=3
			else
				i_1=i
			endif
			
			
			fvec_contact(2*nts_elem_nod(active_nts(j),i_1)-1 ) &
				=fvec_contact(2*nts_elem_nod(active_nts(j),i_1)-1 )+fvec_e(2*i_1-1)
			fvec_contact(2*nts_elem_nod(active_nts(j),i_1)) &
				=fvec_contact(2*nts_elem_nod(active_nts(j),i_1))+fvec_e(2*i_1)	
		enddo		
			
			
		else
		
			 stop  "error :: invalid beta"
		endif


	
	 !諸量の更新
	 nts_amo(active_nts(j),1)     =gz0 !trial gzi0 on current timestep
	 nts_amo(active_nts(j),2)     =dble(beta) !trial beta on current timestep
	 !nts_amo(active_nts(j),10)    =gz !converged gzi at last timestep
	 !nts_amo(active_nts(j),11)    =pn !inactive
	 
	 
     
 
 end subroutine state_stick
 !============================================================
 !check for contact: gn<0 → active NTS-element----------------
 
 subroutine ls_check_active_CM(obj)
	
	class(ContactMechanics_),intent(inout) :: obj
	real(real64),allocatable::nod_coord(:,:)

	integer,allocatable ::check_active_nts(:)
	integer active_nts_max,i,j
	
	allocate(nod_coord(size(obj%old_nod_coord,1),size(obj%old_nod_coord,2)))
	do i=1, size(nod_coord,1)
		nod_coord(i,1)=obj%old_nod_coord(i,1)+obj%uvec(2*i-1)+obj%duvec(2*i-1)
		nod_coord(i,2)=obj%old_nod_coord(i,2)+obj%uvec(2*i  )+obj%duvec(2*i  )
	enddo	
	
	allocate( check_active_nts(size(obj%nts_elem_nod,1) )  )
	do i=1, size(obj%nts_elem_nod,1)
	    call check_gn(i,obj%nts_elem_nod,check_active_nts,nod_coord)
	enddo
	active_nts_max=0
	
	do i=1, size(obj%nts_elem_nod,1 )
	   if( check_active_nts(i)==1 )then
	       active_nts_max=active_nts_max+1 !active
       elseif(check_active_nts(i)==0)then
	       cycle
	   else
	        stop "something wrong at check_active_nts"
	   endif
	enddo
	if( allocated(obj%active_nts) )deallocate(obj%active_nts)
	!print *, "active nts= ",active_nts_max,"/",size(obj%nts_elem_nod,1)
	allocate(obj%active_nts(active_nts_max) )
	
	j=0
	do i=1, size(obj%nts_elem_nod,1)
	   if( check_active_nts(i)==1)then
	      j=j+1
		  obj%active_nts(j)=i
	   else
	      cycle
	   endif
	enddo
	
	
 end subroutine ls_check_active_CM
!=============================================================
!check gn
!-------------------
 subroutine check_gn(j,nts_elem_nod,check_active_nts,nod_coord)
	 real(real64), allocatable ::x2s(:),x11(:),x12(:),avec(:),nvec(:),evec(:),yL(:),tvec_(:),nvec_(:)
	
	real(real64), intent(in) :: nod_coord(:,:)
    integer, intent(in) :: j, nts_elem_nod(:,:)
    real(real64) gz,l,gns,alpha,sel,delta
	integer i,beta
	integer:: check_active_nts(:)
	delta=1.0e-5
	!get beta to determine the case (cf. W.N. Liu et al., 2003)
	call get_beta_st_nts(j,nts_elem_nod,nod_coord,beta)

	allocate(x2s(2),x11(2),x12(2),yL(2),tvec_(3),nvec_(3),avec(3),nvec(3),evec(3))
	 
	if(beta==1)then
		x2s(1:2) = nod_coord(nts_elem_nod(j,1),1:2)
		x11(1:2) = nod_coord(nts_elem_nod(j,2),1:2)
		x12(1:2) = nod_coord(nts_elem_nod(j,3),1:2)
	else
		x2s(1:2) = nod_coord(nts_elem_nod(j,1),1:2)
		x11(1:2) = nod_coord(nts_elem_nod(j,4),1:2)
		x12(1:2) = nod_coord(nts_elem_nod(j,2),1:2)
	endif
	
	
	 ! 0 duvecの格納,ξ,ｇN等諸量の格納
	 !-----------------------------------------------------------------------
	 
     !-----------------------------------------------------------------------

	 nvec(3) = 0.0d0
	 avec(3) = 0.0d0
	 evec(1) = 0.0d0
	 evec(2) = 0.0d0
	 evec(3) = 1.0d0
	 nvec_(3) = 0.0d0
	 tvec_(3) = 0.0d0
	!----------------------------------
	 l = dot_product( x12(1:2)-x11(1:2), x12(1:2)-x11(1:2)) 
	 l=dsqrt(l)

	 if(l==0.0d0)then
		print *, "l=0 at element No.",j
		 stop 
	 endif

	 avec(1:2) = ( x12(1:2)-x11(1:2)  )/l

	 nvec(:) = cross_product(evec,avec)
	 gz=1.0d0/l*dot_product(x2s(1:2)-x11(1:2),avec(1:2) )
	 
	
	 if(beta==1)then
		 !alpha=4.0d0*gz*(1.0d0-gz)
		 !alpha=0.50d0*(1.0d0-cos(2.0d0*3.1415926535d0*gz) )
		 !alpha=exp( -delta*delta*(2.0d0*gz-1.0d0)**2.0d0 )
		 alpha=1.0d0
		 !alpha=0.0d0
		yL(:)=nod_coord(nts_elem_nod(j,4),1:2)+alpha*&
			(nod_coord(nts_elem_nod(j,2),1:2)-nod_coord(nts_elem_nod(j,4),1:2) )
		sel=dsqrt(dot_product(x12-yL,x12-yL))
		
		if(sel==0.0d0)then
			 stop  "error check_gn"
		endif
		tvec_(1:2)=(x12(:)-yL(:) )/sel
		nvec_(:)=cross_product(evec,tvec_)
		 nvec_(:)=nvec_(:)*dble(beta) 
		 gns = dot_product((x2s(:)-x11(:)),nvec_(1:2))
	else
		!alpha=4.0d0*gz*(1.0d0-gz)
		!alpha=0.50d0*(1.0d0-cos(2.0d0*3.1415926535d0*gz) )
		!alpha=exp( -delta*delta*(2.0d0*gz-1.0d0)**2.0d0 )
		alpha=1.0d0
		!alpha=0.0d0
		yL(:)=nod_coord(nts_elem_nod(j,3),1:2)+alpha*&
			(nod_coord(nts_elem_nod(j,2),1:2)-nod_coord(nts_elem_nod(j,3),1:2) )
		sel=dsqrt(dot_product(x11-yL,x11-yL))
		if(sel==0.0d0)then
			 stop  "error check_gn"
		endif
		tvec_(1:2)=(x11(:)-yL(:) )/sel
		nvec_(:)=cross_product(evec,tvec_)
		nvec_(:)=nvec_(:)*dble(beta) 
		gns = dot_product((x2s(:)-x12(:)),nvec_(1:2))
	endif
	
	
	 
	 !gnsの計算と更新-----------------------------------------------------
	 
	 !---------------------------------------------------------------------
	
	 
	 if(gns > 0.0d0)then
	    check_active_nts(j)=0
	 elseif(gns <= 0.0d0)then
	    check_active_nts(j)=1
	 else
	     stop 'invalid No. on check_active_nts'
	 endif
	 
	 deallocate(x2s,x11,x12,avec,nvec,evec)
	  
	  
 end subroutine check_gn

!=============================================================
!update friction
!-------------------
 subroutine update_friction(j,nod_max,nod_coord,nts_elem_nod,active_nts,surface_nod,sur_nod_inf&
              ,nts_amo, k_contact,uvec,duvec,fvec_contact,stick_slip,contact_mat_para,nts_mat,itr_contact)
	 real(real64), allocatable ::x2s(:), dgt(:),tt_tr(:),gslt(:),&
	n_t(:),K_st(:,:),ns(:),n0s(:),ts(:),t0s(:),ngz0(:), &
	x11(:),x12(:),evec(:),gt(:),avec(:),&
	nvec(:),k_sl(:,:),n_tr(:),ts_st(:),ts_sl(:),fvec_e(:),&
	x1(:),x2(:),x3(:),x4(:),x5(:),x6(:),&
	x1_0(:),x2_0(:),x3_0(:),x4_0(:),x5_0(:),x6_0(:),c_nod_coord(:,:),&
	tvec_(:),nvec_(:),xe(:), xL(:),xs_1(:),xs_2(:),xs_0(:)

	real(real64), intent(inout) ::nts_amo(:,:),k_contact(:,:),fvec_contact(:)
	real(real64), intent(in) :: nod_coord(:,:),uvec(:),duvec(:),contact_mat_para(:,:)
    integer, intent(in) :: j, nod_max,nts_elem_nod(:,:),active_nts(:),nts_mat(:),itr_contact
	integer, intent(in) :: surface_nod(:),sur_nod_inf(:,:)
	integer, intent(inout) ::stick_slip(:)
    real(real64) c,phy,en,ct,f_tr,Lamda,gns,gz0,gz,l,pn,f_tr0,x,tts,tol_rmm,signm,beta_0,alpha,sel,gz0_,gz_,c_num,delta
	real(real64) l_s1,l_s2,ls_ave
	integer i, ii ,k,ss,itr_rm,z,gzn,node_ID,beta,shift,old_slave,slave1,slave2
	 
	 ! 0 duvecの格納,ξ,ｇN等諸量の格納
	 tol_rmm=1.0e-12
	 delta=1.0e-5
	 allocate(x2s(2),x11(2),x12(2),evec(3),&
	 dgt(2),gt(2),gslt(2),tt_tr(2),avec(3),nvec(3),k_st(6,6),k_sl(6,6),&
	 ns(6),n0s(6),ts(6),t0s(6),ngz0(6),ts_st(6),ts_sl(6),fvec_e(6) )
	 allocate(x1(2),x2(2),x3(2),x4(2),x5(2),x6(2))
	 allocate(x1_0(2),x2_0(2),x3_0(2),x4_0(2),x5_0(2),x6_0(2))
	 allocate( c_nod_coord(size(nod_coord,1) ,size(nod_coord,2)),tvec_(3),nvec_(3) )
	 allocate(xe(2), xL(2),xs_1(2),xs_2(2),xs_0(2) )
	 tvec_(3)=0.0d0
	 nvec_(3)=0.0d0
     !-----諸量の読み込み------
	 !tts = nts_amo(active_nts(j),12)!初期または全ステップ終了時のξ
	 !gt(1:2)=nts_amo(active_nts(j),2:3)
	 !gslt(1:2)=nts_amo(active_nts(j),4:5)
	 !en=nts_amo(active_nts(j),6)
	! ct = nts_amo(active_nts(j),7)
	 !c = nts_amo(active_nts(j),8)
	! gslt(1:2)=nts_amo(active_nts(j),4:5)
	 tts=nts_amo(active_nts(j),11) !previous step
	 !tts = nts_amo(active_nts(j),12) !converged gz0 at last timestep
	 !beta_0 = nts_amo(active_nts(j),2) !converged gz0 at last timestep
	 !--------------------------
	 !-----材料パラメータの読み込み------
	 en=contact_mat_para(nts_mat( active_nts(j) ),2 )
	 ct=contact_mat_para(nts_mat( active_nts(j) ),1 )
	 c = contact_mat_para(nts_mat( active_nts(j) ),3 )
	 phy=contact_mat_para(nts_mat( active_nts(j) ),4 )
	 !--------------------------
	 
	 do i=1,size(nod_coord,1)
		c_nod_coord(i,1)=nod_coord(i,1)+uvec(2*i-1)+duvec(2*i-1)
		c_nod_coord(i,2)=nod_coord(i,2)+uvec(2*i  )+duvec(2*i  )
	 enddo
	 
!以下、初期座標＋変位により、位置ベクトルを更新し、諸量を更新
	 !gz更新
	 x1(1:2) = uvec(2*nts_elem_nod(active_nts(j),1)-1:&
	    2*nts_elem_nod(active_nts(j),1))+&
		duvec(2*nts_elem_nod(active_nts(j),1)-1:&
	    2*nts_elem_nod(active_nts(j),1))+&
		nod_coord(nts_elem_nod(active_nts(j),1),1:2)
	 x2(1:2) = uvec(2*nts_elem_nod(active_nts(j),2)-1:&
	    2*nts_elem_nod(active_nts(j),2))+&
		duvec(2*nts_elem_nod(active_nts(j),2)-1:&
	    2*nts_elem_nod(active_nts(j),2))+&
		nod_coord(nts_elem_nod(active_nts(j),2),1:2)
	 x3(1:2) = uvec(2*nts_elem_nod(active_nts(j),3)-1:&
	    2*nts_elem_nod(active_nts(j),3))+&
		duvec(2*nts_elem_nod(active_nts(j),3)-1:&
	    2*nts_elem_nod(active_nts(j),3))+&
		nod_coord(nts_elem_nod(active_nts(j),3),1:2)	 
	 x4(1:2) = uvec(2*nts_elem_nod(active_nts(j),4)-1:&
	    2*nts_elem_nod(active_nts(j),4))+&
		duvec(2*nts_elem_nod(active_nts(j),4)-1:&
	    2*nts_elem_nod(active_nts(j),4))+&
		nod_coord(nts_elem_nod(active_nts(j),4),1:2)
	x5(1:2) = uvec(2*nts_elem_nod(active_nts(j),5)-1:&
	    2*nts_elem_nod(active_nts(j),5))+&
		duvec(2*nts_elem_nod(active_nts(j),5)-1:&
	    2*nts_elem_nod(active_nts(j),5))+&
		nod_coord(nts_elem_nod(active_nts(j),5),1:2)
	x6(1:2) = uvec(2*nts_elem_nod(active_nts(j),6)-1:&
	    2*nts_elem_nod(active_nts(j),6))+&
		duvec(2*nts_elem_nod(active_nts(j),6)-1:&
	    2*nts_elem_nod(active_nts(j),6))+&
		nod_coord(nts_elem_nod(active_nts(j),6),1:2)
		
		
	x1_0(1:2) = uvec(2*nts_elem_nod(active_nts(j),1)-1:&
	    2*nts_elem_nod(active_nts(j),1))+&
		nod_coord(nts_elem_nod(active_nts(j),1),1:2)
	 x2_0(1:2) = uvec(2*nts_elem_nod(active_nts(j),2)-1:&
	    2*nts_elem_nod(active_nts(j),2))+&
		nod_coord(nts_elem_nod(active_nts(j),2),1:2)
	 x3_0(1:2) = uvec(2*nts_elem_nod(active_nts(j),3)-1:&
	    2*nts_elem_nod(active_nts(j),3))+&
		nod_coord(nts_elem_nod(active_nts(j),3),1:2)	 
	 x4_0(1:2) = uvec(2*nts_elem_nod(active_nts(j),4)-1:&
	    2*nts_elem_nod(active_nts(j),4))+&
		nod_coord(nts_elem_nod(active_nts(j),4),1:2)
	x5_0(1:2) = uvec(2*nts_elem_nod(active_nts(j),5)-1:&
	    2*nts_elem_nod(active_nts(j),5))+&
		nod_coord(nts_elem_nod(active_nts(j),5),1:2)
	x6_0(1:2) = uvec(2*nts_elem_nod(active_nts(j),6)-1:&
	    2*nts_elem_nod(active_nts(j),6))+&
		nod_coord(nts_elem_nod(active_nts(j),6),1:2)
	 node_ID=active_nts(j)
	 
	 
	call get_beta_st_nts(node_ID,nts_elem_nod,c_nod_coord,beta)

	!============================
	!compute gzi_0
	if(beta==1)then
		x2s(1:2) = x1_0(:)
		x11(1:2) = x2_0(:)
		x12(1:2) = x3_0(:)
	else
		x2s(1:2) = x1_0(:)
		x11(1:2) = x4_0(:)
		x12(1:2) = x2_0(:)
	endif
	 nvec(3) = 0.0d0
	 avec(3) = 0.0d0
	 evec(1) = 0.0d0
	 evec(2) = 0.0d0
	 evec(3) = 1.0d0
	!----------------------------------
	 l = dot_product( x12(1:2)-x11(1:2), &
		 x12(1:2)-x11(1:2)) 
	 l=dsqrt(l)
	 
	 if(l==0.0d0)then
		print *, "l=0 at element No.",j
		 stop 
	 endif
	 if(ct==0.0d0)then

		print *, "ct=0 at element No.",j
		 stop 
	 endif
	 
	 avec(1:2) = ( x12(1:2)-x11(1:2)  )/l
	 
	 nvec(:) = cross_product(evec,avec)
	 nvec(:)=nvec(:)/sqrt(dot_product(nvec,nvec)  )
	 gz0=1.0d0/l*dot_product(x2s(1:2)-x11(1:2),avec(1:2) )	
	 !alpha=0.50d0*(1.0d0-cos(2.0d0*3.1415926535d0*gz0) )
	 !alpha=exp( -delta*delta*(2.0d0*gz0-1.0d0)**2.0d0 )
	 !alpha=1.0d0
	 !alpha=0.0d0
	 if(beta==1)then
		xe(1:2)=x3_0(1:2)
		xL(1:2)=x4_0(1:2)+alpha*( x2_0(1:2) - x4_0(1:2)  )
		tvec_(1:2)=(xe(1:2)-xL(1:2))/dsqrt(dot_product(xe-xL,xe-xL)  )
		nvec_(1:3)=cross_product(evec,tvec_)
		nvec_(1:2)=nvec_(1:2)*dble(beta)
		gns=dot_product( x1_0(1:2)-x2_0(1:2),nvec_(1:2) )
		
	else
		xe(1:2)=x4_0(1:2)
		xL(1:2)=x3_0(1:2)+alpha*( x2_0(1:2) - x3_0(1:2)  )
		tvec_(1:2)=(xe(1:2)-xL(1:2))/dsqrt(dot_product(xe-xL,xe-xL)  )
		nvec_(1:3)=cross_product(evec,tvec_)
		nvec_(1:2)=nvec_(1:2)*dble(beta)
		gns=dot_product( x1_0(1:2)-x2_0(1:2),nvec_(1:2) )
		
		
	endif
	sel=dsqrt(dot_product(xL(1:2)-xe(1:2),xL(1:2)-xe(1:2) ))
	gz0_=1.0d0/sel*dot_product( x1_0(:)-x2_0(:),dble(beta)*tvec_(1:2) )
	
	!=====================
	!compute gzi
	if(beta==1)then
		x2s(1:2) = x1(:)
		x11(1:2) = x2(:)
		x12(1:2) = x3(:)
		
	else
		x2s(1:2) = x1(:)
		x11(1:2) = x4(:)
		x12(1:2) = x2(:)
		
	endif
	 nvec(3) = 0.0d0
	 avec(3) = 0.0d0
	 evec(1) = 0.0d0
	 evec(2) = 0.0d0
	 evec(3) = 1.0d0
	!----------------------------------
	 l = dot_product( x12(1:2)-x11(1:2), &
		 x12(1:2)-x11(1:2)) 
	 l=dsqrt(l)
	 
	 if(l==0.0d0)then
		print *, "l=0 at element No.",j
		 stop 
	 endif
	 if(ct==0.0d0)then

		print *, "ct=0 at element No.",j
		 stop 
	 endif
	 
	 avec(1:2) = ( x12(1:2)-x11(1:2)  )/l
	 
	 nvec(:) = cross_product(evec,avec)
	 nvec(:)=nvec(:)/dsqrt(dot_product(nvec,nvec)  )
	 gz=1.0d0/l*dot_product(x2s(1:2)-x11(1:2),avec(1:2) )
	 !alpha=4.0d0*gz*(1.0d0-gz)
	 !alpha=0.50d0*(1.0d0-cos(2.0d0*3.1415926535d0*gz) )
	 
	 !alpha=exp( -delta*delta*(2.0d0*gz-1.0d0)**2.0d0 )
	 alpha=1.0d0
	!alpha=0.0d0
	!gz0=gz-tts/ct/l
	 !gnsの計算と更新-----------------------------------------------------
	 if(beta==1)then
		xe(1:2)=x3(1:2)
		xL(1:2)=x4(1:2)+alpha*( x2(1:2) - x4(1:2)  )
		tvec_(1:2)=(xe(1:2)-xL(1:2))/dsqrt(dot_product(xe-xL,xe-xL)  )
		nvec_(1:3)=cross_product(evec,tvec_)
		nvec_(1:2)=nvec_(1:2)*dble(beta)
		gns=dot_product( x1(1:2)-x2(1:2),nvec_(1:2) )
		
	else
		xe(1:2)=x4(1:2)
		xL(1:2)=x3(1:2)+alpha*( x2(1:2) - x3(1:2)  )
		tvec_(1:2)=(xe(1:2)-xL(1:2))/dsqrt(dot_product(xe-xL,xe-xL)  )
		nvec_(1:3)=cross_product(evec,tvec_)
		nvec_(1:2)=nvec_(1:2)*dble(beta)
		gns=dot_product( x1(1:2)-x2(1:2),nvec_(1:2) )
		
		
	endif
	 !gns = dot_product((x2s(:)-(1.0d0-gz)*x11(:)-gz*x12(:)),nvec(1:2))
	 sel=dsqrt(dot_product(xL(1:2)-xe(1:2),xL(1:2)-xe(1:2) ))
	 gz_=1.0d0/sel*dot_product( x1(:)-x2(:),dble(beta)*tvec_(1:2) )
	 pn=en*gns
	 
	 
	 !---------------------------------------------------------------------
	 !trial tTs(frictional force)
	 gz0_=gz0_-tts/ct/sel !gzi_0の補正(現時点でのfrictional stress を考慮)
	 tts=ct*( gz_ -gz0_ )*sel ! compute trial tts 
	 
	 
	
	 !------降伏関数の計算------------------------
	 !compute numerical c (kPa)
	 shift=1
	 old_slave=nts_elem_nod(active_nts(j),4)
	 call get_next_segment(surface_nod,sur_nod_inf,shift,old_slave,slave1,slave2)
	 shift=-1
	 old_slave=nts_elem_nod(active_nts(j),4)
	 call get_next_segment(surface_nod,sur_nod_inf,shift,old_slave,slave2,slave1)
	 xs_0(1:2)=uvec(2*old_slave-1:2*old_slave)+&
		duvec(2*old_slave-1:2*old_slave)+&
		nod_coord(old_slave,1:2)
	xs_1(1:2)=uvec(2*slave1-1:2*slave1)+&
		duvec(2*slave1-1:2*slave1)+&
		nod_coord(slave1,1:2)
	xs_2(1:2)=uvec(2*slave2-1:2*slave2)+&
		duvec(2*slave2-1:2*slave2)+&
		nod_coord(slave2,1:2)
	l_s1=dsqrt(dot_product(xs_0-xs_1,xs_0-xs_1)  )
	l_s2=dsqrt(dot_product(xs_0-xs_2,xs_0-xs_2)  )
	ls_ave=0.50d0*(l_s1+l_s2)
	
	c_num=c*ls_ave/l 
	 
	 ! print *,"tts=",tts,pn,sel,l,c,c_num,alpha
	 
	 f_tr0 = abs(tts)-((tan(phy))*abs(pn) + c_num)
	!------------------------------
	 itr_rm=1

	 !----------------------降伏関数の値による場合わけ--------------------
	 if(f_tr0<=0.0d0) then      
		  !print *, "stick"
		  stick_slip( active_nts(j)  )=0
     elseif(f_tr0>0.0d0)then

		 !--------------------plastic------------------------------!
		 !ss=1
		 
		 !allocate(n_tr(2))
		 
	      !do
		      
              !---繰り返し回数計測用変数の更新-------
			!  itr_rm=itr_rm+1
			  
			  !------デバッグ用--itr_rm=5で停止----
			 ! if(itr_rm==5)then
			 !      stop  'itr_rm=5'
			 ! endif

			  !write(1000,*)"slip"
			  !print *, "slip"
			  !------Return Mapping Calculation -----------------------------
			  !------降伏曲面の法線ベクトルnの計算----------------------
			  
			  if(tts>=0.0d0)then
				signm=1.0d0
			  elseif(tts<0.0d0)then
				signm=-1.0d0
			 else
				 stop "invalid tTs"
			  endif
			  stick_slip( active_nts(j)  )=1
			  tts=((tan(phy))*abs(pn)+ c_num)*signm
			  !gz0=gz-tts/ct/l
			 ! n_tr(:)=1.0d0/dsqrt((dot_product(tt_tr,tt_tr)))*tt_tr(:)
			  
			  !塑性指数Lamda,tt_tr,gsltの更新(Computational contact mechanics 
			  
			  !Lamda=1.0d0/ct*(dsqrt(dot_product(tt_tr,tt_tr))&
			  !  -(abs(pn)*tan(phy)+c) ) !pnは負、cは正

			  !gslt(:)=gslt(:)+Lamda*n_tr(:)
			  !tt_tr(:) =ct*( gt(:)-gslt(:)  )
			  !------debug
			   !write(1000,*)'RM itr=',ss
			  !-------      

			  !f_tr = dsqrt(dot_product(tt_tr,tt_tr))-((tan(phy))*abs(pn)+c)

			  !---------------------------------------------------------------
			  !収束判定
			  !   if( abs(f_tr)< tol_rmm*abs(f_tr0))then

				!	 tts=ct*dot_product((gt-gslt),avec)
					 
			!		 deallocate(n_tr) !  stop ' stop  return mapping'
			!		 exit
			 !    else
				     
             !        ss=ss+1
			!	     cycle
			 !    endif
	      !enddo
		  
	  else
	       stop  'something wrong about f_tr0'
	  endif
	 


    !諸量の更新
	 !gz0=gz-tts/ct/l
	 
	! nts_amo(active_nts(j),2:3)  =gt(1:2)
	! nts_amo(active_nts(j),4:5)=gslt(1:2)
	 
	 nts_amo(active_nts(j),12)=tts
	 nts_amo(active_nts(j),10) =signm
	 deallocate(x2s,x11,x12,evec,&
	 gt,gslt,tt_tr,avec,nvec,k_st,k_sl,&
	 ns,n0s,ts,t0s,ngz0,ts_sl,fvec_e)
	 
	 
	 
	  
 end subroutine update_friction

!==============================================================

 subroutine update_res_grad_c_i(j,nod_max,old_nod_coord,nts_elem_nod,active_nts&
              ,nts_amo, k_contact,uvec,duvec,fvec_contact,stick_slip,contact_mat_para,nts_mat)




	real(real64), allocatable ::x2s(:),x11(:),x12(:),evec(:),avec(:),nvec(:)&
	,k_st(:,:),ns(:),n0s(:),ts(:),ts_st(:),t0s(:),ngz0(:),fvec_e(:),nod_coord(:,:),&
	nvec_(:),tvec_(:),x1(:),x2(:),x3(:),x4(:),x5(:),x6(:),tvec(:),mvec(:),yi(:),Dns(:,:),&
	ym(:),ys(:),nvec__(:),ovec(:),mvec_(:),mvec__(:),Dns_1(:),Dns_2(:),Dns_3(:),domega_mat(:),&
	Dns_1_1(:),Ivec(:),dtmat(:,:),dmmat(:,:),dnmat__(:,:),dgzivec(:),dalpha(:),dHvec(:),nt(:),&
	Dnt(:,:),dT0vec(:),dtmat_(:,:),dselvec(:),dmmat_(:,:),dgzi_hat_vec(:),dganma_hat_vec(:),&
	dganmavec_(:),dnmat_(:,:),dgzivec_(:),dsjkvec(:),dlamdavec_(:),Svec(:),Ft(:),yL(:),tvec__(:),&
	ye(:),yj(:),yk(:),c_nod_coord(:,:)
	

	real(real64) ,intent(inout)::nts_amo(:,:),k_contact(:,:),fvec_contact(:)
	real(real64), intent(in) :: old_nod_coord(:,:),uvec(:),contact_mat_para(:,:),duvec(:)
    integer, intent(in) :: j, nod_max,nts_elem_nod(:,:),active_nts(:),nts_mat(:)
	integer, intent(inout) :: stick_slip(:)
    real(real64) c,phy,en,ct,gns,gz,l,pn,tts,gt,gz0,alpha,omega,gns_,gz_,sjk,delta
	real(real64) gzi_hat,delta_hat,ganma_,kappa,S0,ganma,gzi_,ganma_hat,lamda_,T0,dfdtn,HH,sel
	integer i, ii , k,beta,i_1,ii_1,node_ID
	 
	 ! 0 duvecの格納,ξ,ｇN等諸量の格納
	 allocate(x2s(2),x11(2),x12(2),evec(3),avec(3),nvec(3),k_st(8,8),&
	 ns(8),n0s(8),ts(8),t0s(8),ngz0(8),ts_st(8),fvec_e(8),tvec(2),mvec(2) )
	 allocate( nvec_(3),tvec_(3),x1(2),x2(2),x3(2),x4(2),x5(2),x6(2),yi(2) )
	 allocate(ym(2),ys(2),nvec__(2),ovec(2),mvec_(2),mvec__(2),Dns(8,8),Dns_1_1(8) )
	 allocate(Dns_1(8),domega_mat(8),Ivec(2) )
	allocate(dtmat(2,8),dmmat(2,8),dnmat__(2,8),dgzivec(8),dalpha(8),dHvec(8) )
	allocate(nod_coord(size(old_nod_coord,1),size(old_nod_coord,2)))
	allocate(nt(8),Dnt(8,8),dT0vec(8),dtmat_(2,8),dselvec(8),dmmat_(2,8),dgzi_hat_vec(8)  )
	allocate( dganma_hat_vec(8),dganmavec_(8),dnmat_(2,8),dgzivec_(8),dsjkvec(8),dlamdavec_(8)  )
	allocate(Svec(8),Ft(8),yL(1:2),tvec__(1:2) )
	allocate(ye(2),yj(2),yk(2),c_nod_coord(size(nod_coord,1),size(nod_coord,2)  ) )
	do i=1, size(nod_coord,1)
		nod_coord(i,1)=old_nod_coord(i,1)
		nod_coord(i,2)=old_nod_coord(i,2)
	enddo
	do i=1,size(nod_coord,1)
		c_nod_coord(i,1)=nod_coord(i,1)+uvec(2*i-1)
		c_nod_coord(i,2)=nod_coord(i,2)+uvec(2*i  )
	 enddo
	 nvec_(3)=0.0d0
	 tvec_(3)=0.0d0
	 
	 delta=1.0e-5
	 !-----材料パラメータの読み込み------
	 en=contact_mat_para(nts_mat( active_nts(j) ),2 )
	 ct=contact_mat_para(nts_mat( active_nts(j) ),1 )
	 c = contact_mat_para(nts_mat( active_nts(j) ),3 )
	 phy=contact_mat_para(nts_mat( active_nts(j) ),4 )
	 !--------------------------------
	  
	 tts=nts_amo(active_nts(j),12) 
	 !dfdtn=nts_amo(active_nts(j),10) 
	  if(tts>=0.0d0)then
		dfdtn=1.0d0
	  elseif(tts<0.0d0)then
		dfdtn=-1.0d0
	 else
		 stop "invalid tTs"
	  endif
	 !以下、初期座標＋変位により、位置ベクトルを更新し、諸量を更新
	 !gz更新
	 x1(1:2) = uvec(2*nts_elem_nod(active_nts(j),1)-1:&
	    2*nts_elem_nod(active_nts(j),1))+&
		duvec(2*nts_elem_nod(active_nts(j),1)-1:&
	    2*nts_elem_nod(active_nts(j),1))+&
		nod_coord(nts_elem_nod(active_nts(j),1),1:2)
	 x2(1:2) = uvec(2*nts_elem_nod(active_nts(j),2)-1:&
	    2*nts_elem_nod(active_nts(j),2))+&
		duvec(2*nts_elem_nod(active_nts(j),2)-1:&
	    2*nts_elem_nod(active_nts(j),2))+&
		nod_coord(nts_elem_nod(active_nts(j),2),1:2)
	 x3(1:2) = uvec(2*nts_elem_nod(active_nts(j),3)-1:&
	    2*nts_elem_nod(active_nts(j),3))+&
		duvec(2*nts_elem_nod(active_nts(j),3)-1:&
	    2*nts_elem_nod(active_nts(j),3))+&
		nod_coord(nts_elem_nod(active_nts(j),3),1:2)	 
	 x4(1:2) = uvec(2*nts_elem_nod(active_nts(j),4)-1:&
	    2*nts_elem_nod(active_nts(j),4))+&
		duvec(2*nts_elem_nod(active_nts(j),4)-1:&
	    2*nts_elem_nod(active_nts(j),4))+&
		nod_coord(nts_elem_nod(active_nts(j),4),1:2)
	x5(1:2) = uvec(2*nts_elem_nod(active_nts(j),5)-1:&
	    2*nts_elem_nod(active_nts(j),5))+&
		duvec(2*nts_elem_nod(active_nts(j),5)-1:&
	    2*nts_elem_nod(active_nts(j),5))+&
		nod_coord(nts_elem_nod(active_nts(j),5),1:2)
	x6(1:2) = uvec(2*nts_elem_nod(active_nts(j),6)-1:&
	    2*nts_elem_nod(active_nts(j),6))+&
		duvec(2*nts_elem_nod(active_nts(j),6)-1:&
	    2*nts_elem_nod(active_nts(j),6))+&
		nod_coord(nts_elem_nod(active_nts(j),6),1:2)
	 node_ID=active_nts(j)
	 
	 
	call get_beta_st_nts(node_ID,nts_elem_nod,c_nod_coord,beta)
	if(beta==1)then
		x2s(1:2) = x1(:)
		x11(1:2) = x2(:)
		x12(1:2) = x3(:)
		yi(1:2) = x4(:)
		yj(1:2) = x2(1:2)
		yk(1:2) = x3(1:2)
		ys(1:2) = x1(1:2)
		ym(1:2) = x2(1:2)
		ye(1:2) = x3(1:2)
		
		
		
	else
		x2s(1:2) = x1(:)
		x11(1:2) = x4(:)
		x12(1:2) = x2(:)
		yi(1:2) = x3(:)
		yj(1:2) = x4(1:2)
		yk(1:2) = x2(1:2)
		ys(1:2) = x1(1:2)
		ym(1:2) = x2(1:2)
		ye(1:2) = x4(1:2)
		
	endif
	
!-----------------------------------------------------------------------

	 nvec(3) = 0.0d0
	 
	 avec(3) = 0.0d0
	 
	 evec(1) = 0.0d0
	 evec(2) = 0.0d0
	 evec(3) = 1.0d0
	 
	 Ivec(1) = 1.0d0
	 Ivec(2) = 1.0d0
	 
	 nvec_(3) = 0.0d0
	 tvec_(3) = 0.0d0
	!----------------------------------
	 l = dot_product( yj(1:2)-yk(1:2), yj(1:2)-yk(1:2)) 
	 l=dsqrt(l)
	sjk=l
	 if(l==0.0d0)then
		print *, "l=0 at element No.",node_ID
		 stop 
	 endif
	
	avec(1:2) = ( yk(1:2)-yj(1:2)  )/l

	 nvec(:) = cross_product(evec,avec)
	 gz=1.0d0/l*dot_product(ys(1:2)-yj(1:2),avec(1:2) )
	 gns = dot_product((ys(:)-ym(:)),nvec(1:2))
	 
	 

	! alpha=4.0d0*gz*(1.0d0-gz)
	 !alpha=0.50d0*(1.0d0-cos(2.0d0*3.1415926535d0*gz) )
	 !alpha=exp( -delta*delta*(2.0d0*gz-1.0d0)**2.0d0 )
	 alpha=1.0d0
	 !alpha=0.0d0
	 yL(:)=yi(:)+alpha*(ym(:)-yi(:))
	 sel=dsqrt(dot_product(ye-yL,ye-yL))
	 gz0=gz-tts/ct/sel

	 if(sel==0.0d0)then
			 stop  "error check_gn"
	endif
	tvec_(1:2)=(ye(:)-yL(:) )/sel
	nvec_(:)=cross_product(evec,tvec_)
	tvec(1:2)=avec(1:2)
	mvec(:)=gz*tvec(:)-gns/sjk*nvec(:)
	nvec__(1:2)=nvec_(1:2)*dble(beta) 
	 
	 !gnsの計算と更新-----------------------------------------------------
	 gns_ = dot_product((ys(:)-ym(:)),nvec__(1:2))	 
	 gz_=1.0d0/sel*dot_product(ys-ym,tvec_(1:2) )
	 
	 !get f_contact(normal),K_contact(normal)
	 !compute common variables
	 gzi_=1.0d0/sel*dot_product(ys-ym,tvec_(1:2) )
	 ganma_hat=1.0d0/sel*dot_product(ym-yi,nvec_(1:2) )
	 !HH=4.0d0*(1.0d0-2.0d0*gz)
	 !HH=-3.1415926535d0*cos(2.0d0*3.1415926535d0*gz)
	 HH=alpha*(delta*delta)*(4.0d0-8.0d0*gz)
	 HH=0.0d0
	 omega=1.0d0/sjk*HH*gz_*dot_product(ym-yi,nvec__(1:2) )
	 
	 gzi_hat=1.0d0/sel*dot_product(ym-yi,tvec_(1:2) )
	 delta_hat=dot_product(ym-yi,nvec_(1:2) )
	 ganma_=1.0d0/sel*dot_product(ys-ym,nvec_(1:2) )
	
	 ganma=gns/sjk
	 ovec(1:2)=gz*nvec(1:2)+ganma*tvec(1:2)
	 mvec_(1:2)=gzi_*tvec_(1:2)-ganma_*nvec_(1:2)
	 mvec__(1:2)=gzi_*tvec_(1:2)-ganma_*nvec_(1:2)
	 !kappa=-8.0d0
	 !kappa=-2.0d0*3.1415926535d0*3.1415926535d0*cos(2.0d0*3.1415926535d0*gz)
	 kappa=alpha*(delta)*(delta)*(delta)*(delta)*(4.0d0-8.0d0*gz) - 8.0d0*alpha*(delta*delta)
	 kappa=0.0d0
	 
	 !kappa=8.0d0
	 tvec__(1:2)=dble(beta)*tvec_(1:2)
	 S0=delta_hat*dble(beta)/sjk*( kappa*gzi_+HH*HH*(2.0d0*gzi_*gzi_hat-ganma_*ganma_hat)  )
	 
	 if(beta==1)then
		!normal part >>>
		ns(1:2)=omega*(tvec(1:2)  )
		ns(3:4)=omega*(mvec(1:2)-tvec(1:2))!!+-
		ns(5:6)=omega*(-mvec(1:2)  )
		ns(7:8)=0.0d0
	 
		ns(1:2)=ns(1:2)+nvec__(1:2)
		ns(3:4)=ns(3:4)-(1.0d0-alpha*gz_)*nvec__(1:2) 
		ns(5:6)=ns(5:6)-gz_*nvec__(1:2)
		ns(7:8)=ns(7:8)+(1.0d0- alpha)*gz_*nvec__(1:2)
		
		Dns_1_1(1:2)=tvec(1:2)
		Dns_1_1(3:4)=mvec(1:2)-tvec(1:2)
		Dns_1_1(5:6)=-mvec(:)
		Dns_1_1(7:8)=0.0d0
		
		domega_mat(1:2)=1.0d0/sjk*S0*tvec(:)
		domega_mat(3:4)=1.0d0/sjk*(S0*(mvec(1:2)-tvec(1:2))+omega*tvec(1:2))!!+-
		domega_mat(5:6)=1.0d0/sjk*(-S0*mvec(1:2)-omega*tvec(1:2))
		domega_mat(7:8)=0.0d0
		
		domega_mat(1:2)=domega_mat(1:2)+HH/sjk*ganma_hat*tvec__(1:2)
		domega_mat(3:4)=domega_mat(3:4)+HH/sjk*( ganma_hat*(alpha*mvec__(1:2)-tvec__(1:2) )+gzi_*(1.0d0+alpha*gzi_hat)*nvec__(1:2))
		domega_mat(5:6)=domega_mat(5:6)+HH/sjk*(-ganma_hat*mvec__(1:2)-gzi_*ganma_hat*nvec__(1:2) ) 
		domega_mat(7:8)=domega_mat(7:8)+HH/sjk*(gzi_*(-1.0d0+(1.0d0-alpha )*gzi_hat)*nvec__(1:2)+ganma_hat*(1.0d0-alpha)*mvec__(1:2)  )
		
		dtmat(1:2,1:2)=0.0d0
		dtmat(1:2,3:4)=-diadic(nvec(1:2),nvec(1:2) )/sjk!!+-
		dtmat(1:2,5:6)=diadic(nvec(1:2),nvec(1:2) )/sjk
		dtmat(1:2,7:8)=0.0d0
		
		dmmat(1:2,1:2)=(diadic(tvec(1:2),tvec(1:2))-diadic(nvec(1:2),nvec(1:2) ))/sjk!!+-
		dmmat(1:2,3:4)=(-diadic(tvec(1:2),tvec(1:2))+diadic(nvec(1:2),nvec(1:2) )+diadic(tvec(1:2),mvec(1:2) )&
			-diadic(nvec(1:2),ovec(1:2))-diadic(ovec(1:2),nvec(1:2) ) )/sjk
		dmmat(1:2,5:6)=(-diadic(tvec(1:2),mvec(1:2) )&
			+diadic(nvec(1:2),ovec(1:2))+diadic(ovec(1:2),nvec(1:2) ) )/sjk
		dmmat(1:2,7:8)=0.0d0
		
		dnmat__(1:2,1:2)=HH*ganma_hat/sjk*diadic(tvec__(1:2),tvec(1:2) )
		dnmat__(1:2,3:4)=HH*ganma_hat/sjk*diadic(tvec__(1:2),mvec(1:2)-tvec(1:2) )!!+-
		dnmat__(1:2,5:6)=-HH*ganma_hat/sjk*diadic(tvec__(1:2),mvec(1:2))
		dnmat__(1:2,7:8)=0.0d0
		
		dnmat__(1:2,1:2)=dnmat__(1:2,1:2)+0.0d0
		dnmat__(1:2,3:4)=dnmat__(1:2,3:4)+1.0d0/sel*alpha*diadic(tvec__(1:2),nvec_(1:2) ) 
		dnmat__(1:2,5:6)=dnmat__(1:2,5:6)-1.0d0/sel*diadic(tvec__(1:2),nvec_(1:2) )
		dnmat__(1:2,7:8)=dnmat__(1:2,7:8)+1.0d0/sel*(1.0d0-alpha)*diadic(tvec__(1:2),nvec_(1:2) )
		 
		dgzivec(1:2)=1.0d0/sjk*tvec(1:2)
		dgzivec(3:4)=1.0d0/sjk*( mvec(1:2)-tvec(1:2) )!!+-
		dgzivec(5:6)=-1.0d0/sjk*mvec(1:2)
		dgzivec(7:8)=0.0d0
		
		dalpha(1:2)=HH/sjk*tvec(1:2)
		dalpha(3:4)=HH/sjk*( mvec(1:2)-tvec(1:2) )!!+-
		dalpha(5:6)=-HH/sjk*mvec(1:2)
		dalpha(7:8)=0.0d0
		
		dHvec(1:2)=kappa/sjk*tvec(1:2)
		dHvec(3:4)=kappa/sjk*( mvec(1:2)-tvec(1:2) )!!+-
		dHvec(5:6)=-kappa/sjk*mvec(1:2)
		dHvec(7:8)=0.0d0
		
		dgzivec_(1:2)=HH*(gzi_*gzi_hat-ganma_*ganma_hat)/sjk*tvec(1:2)
		dgzivec_(3:4)=HH*(gzi_*gzi_hat-ganma_*ganma_hat)/sjk*(mvec(1:2)-tvec(1:2) )!!+-
		dgzivec_(5:6)=-HH*(gzi_*gzi_hat-ganma_*ganma_hat)/sjk*mvec(1:2)
		dgzivec_(7:8)=0.0d0
		
		dgzivec_(1:2)=dgzivec_(1:2)+1.0d0/sel*tvec_(1:2)
		dgzivec_(3:4)=dgzivec_(3:4)+1.0d0/sel*(alpha*mvec_(1:2)-tvec_(1:2)) 
		dgzivec_(5:6)=dgzivec_(5:6)+1.0d0/sel*(-1.0d0)*mvec_(1:2)
		dgzivec_(7:8)=dgzivec_(7:8)+1.0d0/sel*(1.0d0-alpha)*mvec_(1:2)
		
		Dns(1:8,1:8)=diadic(Dns_1_1,domega_mat)
		
		
		Dns(1:2,1:8)=Dns(1:2,1:8)+omega*dtmat(1:2,1:8)
		Dns(3:4,1:8)=Dns(3:4,1:8)+omega*( dmmat(1:2,1:8)-dtmat(1:2,1:8) )!!+-
		Dns(5:6,1:8)=Dns(5:6,1:8)+omega*(-dmmat(1:2,1:8) )
		Dns(7:8,1:8)=Dns(7:8,1:8)+0.0d0
		
		Dns(1:2,1:8)=Dns(1:2,1:8)+dnmat__(1:2,1:8)
		Dns(3:4,1:8)=Dns(3:4,1:8)+diadic(nvec__(1:2),alpha*dgzivec_(1:8)+gzi_*dalpha(1:8) )-(1.0d0-alpha*gzi_)*dnmat__(1:2,1:8) 
		Dns(5:6,1:8)=Dns(5:6,1:8)-diadic(nvec__(1:2),dgzivec_(1:8) )-gzi_*dnmat__(1:2,1:8)
		Dns(7:8,1:8)=Dns(7:8,1:8)+diadic(nvec__(1:2),(1.0d0-alpha)*dgzivec_(1:8)-gzi_*dalpha(1:8) )+(1.0d0-alpha)*gzi_*dnmat__(1:2,1:8)
		
		
		fvec_e(1:8)= en*gns_*ns(1:8)
		K_st(1:8,1:8)=en*(diadic(ns,ns)+gns_*Dns(1:8,1:8) )
		! note >> du(1),du(2),du(3),du(4)
		
		!tangential part>>>
		
	
		dnmat_(1:2,1:2)=HH*ganma_hat/sjk*diadic(tvec_(1:2),tvec(1:2) )
		dnmat_(1:2,3:4)=HH*ganma_hat/sjk*diadic(tvec_(1:2),mvec(1:2)-tvec(1:2) )!!+-
		dnmat_(1:2,5:6)=-HH*ganma_hat/sjk*diadic(tvec_(1:2),mvec(1:2))
		dnmat_(1:2,7:8)=0.0d0
		
		dnmat_(1:2,1:2)=dnmat__(1:2,1:2)+0.0d0
		dnmat_(1:2,3:4)=dnmat__(1:2,3:4)+1.0d0/sel*alpha*diadic(tvec_(1:2),nvec_(1:2) ) 
		dnmat_(1:2,5:6)=dnmat__(1:2,5:6)-1.0d0/sel*diadic(tvec_(1:2),nvec_(1:2) )
		dnmat_(1:2,7:8)=dnmat__(1:2,7:8)+1.0d0/sel*(1.0d0-alpha)*diadic(tvec_(1:2),nvec_(1:2) )
		 
		
		dganmavec_(1:2)=HH*(gzi_*ganma_hat+gzi_hat*ganma_)/sjk*(tvec(1:2))
		dganmavec_(3:4)=HH*(gzi_*ganma_hat+gzi_hat*ganma_)/sjk*(mvec(1:2)-tvec(1:2))!!+-
		dganmavec_(5:6)=HH*(gzi_*ganma_hat+gzi_hat*ganma_)/sjk*(-mvec(1:2))
		dganmavec_(7:8)=0.0d0
		
		dganmavec_(1:2)=dganmavec_(1:2)+1.0d0/sel*(nvec_(1:2))
		dganmavec_(3:4)=dganmavec_(3:4)+1.0d0/sel*(alpha*(gzi_*nvec_(1:2)+ganma_*tvec_(1:2))-nvec_(1:2) ) 
		dganmavec_(5:6)=dganmavec_(5:6)+1.0d0/sel*(-(gzi_*nvec_(1:2)+ganma_*tvec_(1:2) ))
		dganmavec_(7:8)=dganmavec_(7:8)+1.0d0/sel*((1.0d0-alpha)*(gzi_*nvec_(1:2)+ganma_*tvec_(1:2)))
		
		dganma_hat_vec(1:2)=2.0d0*HH*gzi_hat*ganma_hat/sjk*(tvec(1:2))
		dganma_hat_vec(3:4)=2.0d0*HH*gzi_hat*ganma_hat/sjk*(mvec(1:2)-tvec(1:2))!!+-
		dganma_hat_vec(5:6)=2.0d0*HH*gzi_hat*ganma_hat/sjk*(-mvec(1:2))
		dganma_hat_vec(7:8)=0.0d0
		
		
		dganma_hat_vec(1:2)=dganma_hat_vec(1:2)+0.0d0
		dganma_hat_vec(3:4)=dganma_hat_vec(3:4)+1.0d0/sel*(alpha*(gzi_hat*nvec_(1:2)+ganma_hat*tvec_(1:2))+nvec_(1:2) ) 
		dganma_hat_vec(5:6)=dganma_hat_vec(5:6)+1.0d0/sel*(-(gzi_hat*nvec_(1:2)+ganma_hat*tvec_(1:2) ))
		dganma_hat_vec(7:8)=dganma_hat_vec(7:8)+1.0d0/sel*((1.0d0-alpha)*(gzi_hat*nvec_(1:2)+ganma_hat*tvec_(1:2))-nvec_(1:2) )
		
		dgzi_hat_vec(1:2)=HH*(gzi_hat*gzi_hat-ganma_hat*ganma_hat)/sjk*(tvec(1:2))
		dgzi_hat_vec(3:4)=HH*(gzi_hat*gzi_hat-ganma_hat*ganma_hat)/sjk*(mvec(1:2)-tvec(1:2))!!+-
		dgzi_hat_vec(5:6)=HH*(gzi_hat*gzi_hat-ganma_hat*ganma_hat)/sjk*(-mvec(1:2))
		dgzi_hat_vec(7:8)=0.0d0
		
		dgzi_hat_vec(1:2)=dgzi_hat_vec(1:2)+0.0d0
		dgzi_hat_vec(3:4)=dgzi_hat_vec(3:4)+1.0d0/sel*((gzi_hat*tvec_(1:2)-ganma_hat*nvec_(1:2))*alpha+tvec_(1:2) )
		dgzi_hat_vec(5:6)=dgzi_hat_vec(5:6)+1.0d0/sel*(-(gzi_hat*tvec_(1:2)-ganma_hat*nvec_(1:2)) ) 
		dgzi_hat_vec(7:8)=dgzi_hat_vec(7:8)+1.0d0/sel*((1.0d0-alpha)*(gzi_hat*tvec_(1:2)-ganma_hat*nvec_(1:2))-tvec_(1:2) )
		
		
		
		
		dtmat_(1:2,1:2)=HH*ganma_hat/sjk*(-1.0d0)*(diadic( nvec_(1:2),tvec(1:2) ) )
		dtmat_(1:2,3:4)=HH*ganma_hat/sjk*(-1.0d0)*(diadic( nvec_(1:2),mvec(1:2)-tvec(1:2) ) )!!+-
		dtmat_(1:2,5:6)=HH*ganma_hat/sjk*(1.0d0)*(diadic( nvec_(1:2),mvec(1:2) ) )
		dtmat_(1:2,7:8)=0.0d0
	
		dtmat_(1:2,1:2)=dtmat_(1:2,1:2)+0.0d0
		dtmat_(1:2,3:4)=dtmat_(1:2,3:4)+1.0d0/sel*(-1.0d0)*alpha*diadic( nvec_(1:2), nvec_(1:2) ) 
		dtmat_(1:2,5:6)=dtmat_(1:2,5:6)+1.0d0/sel*(1.0d0)*diadic( nvec_(1:2), nvec_(1:2) )
		dtmat_(1:2,7:8)=dtmat_(1:2,7:8)+1.0d0/sel*(-1.0d0+alpha)*diadic( nvec_(1:2), nvec_(1:2) )
		
		
		
		
		dmmat_(1:2,1:8)=diadic(tvec_(1:2),dgzivec_(1:8) )
		
		dmmat_(1:2,1:8)=dmmat_(1:2,1:8)+gzi_*dtmat_(1:2,1:8)
		
		dmmat_(1:2,1:8)=dmmat_(1:2,1:8)+diadic( nvec_(1:2), dganmavec_(1:8))
		
		dmmat_(1:2,1:8)=dmmat_(1:2,1:8)+ganma_*dnmat_(1:2,1:8)
		
		dselvec(1:2)=sel*HH*gzi_hat/sjk*(-1.0d0)*tvec(1:2)
		dselvec(3:4)=sel*HH*gzi_hat/sjk*(-1.0d0)*(mvec(1:2)-tvec(1:2))
		dselvec(5:6)=sel*HH*gzi_hat/sjk*mvec(1:2)
		dselvec(7:8)=0.0d0
		
		dselvec(1:2)=dselvec(1:2)+0.0d0
		dselvec(3:4)=dselvec(3:4)+(-alpha)*tvec_(1:2) !!+-
		dselvec(5:6)=dselvec(5:6)+tvec_(1:2)
		dselvec(7:8)=dselvec(7:8)-(1.0d0-alpha)*tvec_(1:2)
		
		dlamdavec_(1:8)=gzi_*dgzi_hat_vec(1:8)+gzi_hat*dgzivec_(1:8)&
			-ganma_hat*dganmavec_(1:8)-ganma_*dganma_hat_vec(1:8)
		
		!original part
		dsjkvec(1:2)=dble(beta)*0.0d0
		dsjkvec(3:4)=dble(beta)*(-1.0d0)*tvec(1:2)
		dsjkvec(5:6)=dble(beta)*tvec(1:2)
		dsjkvec(7:8)=dble(beta)*0.0d0
		
		lamda_=gzi_*gzi_hat-ganma_*ganma_hat
		T0=1.0d0/sjk*HH*lamda_
		
		dT0vec(1:8)=-HH*lamda_/sjk/sjk*dsjkvec(1:8)+HH/sjk*dlamdavec_(1:8)+lamda_/sjk*dHvec(1:8)
		
		Svec(1:2)=-sel*HH*gzi_hat/sjk*tvec(1:2)
		Svec(3:4)=-sel*HH*gzi_hat/sjk*mvec(1:2)-tvec(1:2)!!+-
		Svec(5:6)=-sel*HH*gzi_hat/sjk*(-mvec(1:2))
		Svec(7:8)=0.0d0
		
		Svec(1:2)=Svec(1:2)+0.0d0
		Svec(3:4)=Svec(3:4)+(-alpha)*tvec_(1:2)  
		Svec(5:6)=Svec(5:6)+tvec_(1:2)
		Svec(7:8)=Svec(7:8)-(1.0d0-alpha)*tvec_(1:2)
		
		nt(1:2)=T0*tvec(1:2)
		nt(3:4)=T0*( mvec(1:2)-tvec(1:2)     )!!+-
		nt(5:6)=T0*(-mvec(1:2) )
		nt(7:8)=0.0d0		
			
		nt(1:2)=nt(1:2)+1.0d0/sel*tvec_(1:2)
		nt(3:4)=nt(3:4)+1.0d0/sel*( alpha*mvec_(1:2)-tvec_(1:2)  ) 
		nt(5:6)=nt(5:6)+1.0d0/sel*(-mvec_(1:2))
		nt(7:8)=nt(7:8)+1.0d0/sel*(1.0d0-alpha )*mvec_(1:2)		

		
		Dnt(1:2,1:8)=diadic(tvec(1:2),dT0vec(1:8) )+T0*dtmat(1:2,1:8)
		Dnt(3:4,1:8)=diadic(mvec(1:2)-tvec(1:2),dT0vec(1:8) )+T0*(dmmat(1:2,1:8)  -dtmat(1:2,1:8))!!+-
		Dnt(5:6,1:8)=-diadic(mvec(1:2),dT0vec(1:8) )-T0*dmmat(1:2,1:8)
		Dnt(7:8,1:8)=0.0d0
		
		Dnt(1:2,1:8)=Dnt(1:2,1:8)-1.0d0/sel/sel*diadic(tvec_(1:2),dselvec(1:8) )
		Dnt(3:4,1:8)=Dnt(3:4,1:8)-1.0d0/sel/sel*diadic( alpha*mvec_(1:2)- tvec_(1:2),dselvec(1:8) ) !inverse original
		Dnt(5:6,1:8)=Dnt(5:6,1:8)-1.0d0/sel/sel*diadic(-mvec_(1:2),dselvec(1:8) )
		Dnt(7:8,1:8)=Dnt(7:8,1:8)-1.0d0/sel/sel*diadic( (1.0d0-alpha)*mvec_(1:2),dselvec(1:8) )
		
		Dnt(1:2,1:8)=Dnt(1:2,1:8)+1.0d0/sel*dtmat_(1:2,1:8)
		Dnt(3:4,1:8)=Dnt(3:4,1:8)+1.0d0/sel*(diadic(mvec_(1:2),dalpha(1:8) )+alpha*dmmat_(1:2,1:8)-dtmat_(1:2,1:8))!inverse original
		Dnt(5:6,1:8)=Dnt(5:6,1:8)+1.0d0/sel*(-dmmat_(1:2,1:8) )
		Dnt(7:8,1:8)=Dnt(7:8,1:8)+1.0d0/sel*(-diadic(mvec_(1:2),dalpha(1:8) )+(1.0d0-alpha)*dmmat_(1:2,1:8))
		
		
		if(stick_slip( active_nts(j)  )==0  )then
			Ft(1:8)=dble(beta)*ct*sel*nt(1:8)
		elseif(stick_slip( active_nts(j)  )==1  )then
			Ft(1:8)=en*tan(phy)*ns(1:8)
		else
			 stop  "invalid stick_slip on contact.f95"
		endif
		fvec_e(1:8)= fvec_e(1:8)+dble(beta)*tts*sel*nt(1:8)
		K_st(1:8,1:8)=K_st(1:8,1:8)+dble(beta)*transpose( sel*diadic(Ft(1:8),nt(1:8))+tts*diadic(Svec(1:8),nt(1:8))+tts*sel*Dnt(1:8,1:8))
		fvec_e(:)=fvec_e(:)*l !integration
		K_st(:,:)=K_st(:,:)*l !integration
		do i = 1,4
			do ii = 1, 4
			
			k_contact(2*nts_elem_nod(active_nts(j),i)-1,2*nts_elem_nod(active_nts(j),ii)-1) &
			=k_contact(2*nts_elem_nod(active_nts(j),i)-1,2*nts_elem_nod(active_nts(j),ii)-1) &
			+k_st(2*i-1,2*ii-1)
			k_contact(2*nts_elem_nod(active_nts(j),i)-1,2*nts_elem_nod(active_nts(j),ii)) &
			=k_contact(2*nts_elem_nod(active_nts(j),i)-1,2*nts_elem_nod(active_nts(j),ii)) &
			+k_st(2*i-1,2*ii)
			k_contact(2*nts_elem_nod(active_nts(j),i),2*nts_elem_nod(active_nts(j),ii)-1) &
			=k_contact(2*nts_elem_nod(active_nts(j),i),2*nts_elem_nod(active_nts(j),ii)-1) &
			+k_st(2*i,2*ii-1)
			k_contact(2*nts_elem_nod(active_nts(j),i),2*nts_elem_nod(active_nts(j),ii)) &
			=k_contact(2*nts_elem_nod(active_nts(j),i),2*nts_elem_nod(active_nts(j),ii)) &
			+k_st(2*i,2*ii)
		
			enddo
		enddo
		
		do i=1,4
			fvec_contact(2*nts_elem_nod(active_nts(j),i)-1 ) &
				=fvec_contact(2*nts_elem_nod(active_nts(j),i)-1 )+fvec_e(2*i-1)
			fvec_contact(2*nts_elem_nod(active_nts(j),i)) &
				=fvec_contact(2*nts_elem_nod(active_nts(j),i))+fvec_e(2*i)	
		enddo
	

	

		
	 elseif(beta==-1)then
		!normal part >>>
		!normal part >>>
		ns(1:2)=omega*(tvec(1:2)  )
		ns(3:4)=omega*(-mvec(1:2)  )
		ns(5:6)=omega*(mvec(1:2)-tvec(1:2))!!+-
		ns(7:8)=0.0d0
	 
		ns(1:2)=ns(1:2)+nvec__(1:2)
		ns(3:4)=ns(3:4)-(1.0d0-alpha*gz_)*nvec__(1:2) 
		ns(5:6)=ns(5:6)-gz_*nvec__(1:2)
		ns(7:8)=ns(7:8)+(1.0d0- alpha)*gz_*nvec__(1:2)
		
		Dns_1_1(1:2)=tvec(1:2)
		Dns_1_1(3:4)=-mvec(:)
		Dns_1_1(5:6)=mvec(1:2)-tvec(1:2)!!+-
		Dns_1_1(7:8)=0.0d0
		
		domega_mat(1:2)=1.0d0/sjk*S0*tvec(:)
		domega_mat(3:4)=1.0d0/sjk*(-S0*mvec(1:2)-omega*tvec(1:2))
		domega_mat(5:6)=1.0d0/sjk*(S0*(mvec(1:2)-tvec(1:2))+omega*tvec(1:2))!!+-
		domega_mat(7:8)=0.0d0
		
		domega_mat(1:2)=domega_mat(1:2)+HH/sjk*ganma_hat*tvec__(1:2)
		domega_mat(3:4)=domega_mat(3:4)+HH/sjk*( ganma_hat*(alpha*mvec__(1:2)-tvec__(1:2) )+gzi_*(1.0d0+alpha*gzi_hat)*nvec__(1:2))
		domega_mat(5:6)=domega_mat(5:6)+HH/sjk*(-ganma_hat*mvec__(1:2)-gzi_*ganma_hat*nvec__(1:2) ) 
		domega_mat(7:8)=domega_mat(7:8)+HH/sjk*(gzi_*(-1.0d0+(1.0d0-alpha )*gzi_hat)*nvec__(1:2)+ganma_hat*(1.0d0-alpha)*mvec__(1:2)  )
		
		dtmat(1:2,1:2)=0.0d0
		dtmat(1:2,3:4)=diadic(nvec(1:2),nvec(1:2) )/sjk
		dtmat(1:2,5:6)=-diadic(nvec(1:2),nvec(1:2) )/sjk!!+-
		dtmat(1:2,7:8)=0.0d0
		
		dmmat(1:2,1:2)=(diadic(tvec(1:2),tvec(1:2))-diadic(nvec(1:2),nvec(1:2) ))/sjk
		dmmat(1:2,3:4)=(-diadic(tvec(1:2),mvec(1:2) )&
			+diadic(nvec(1:2),ovec(1:2))+diadic(ovec(1:2),nvec(1:2) ) )/sjk
		dmmat(1:2,5:6)=(-diadic(tvec(1:2),tvec(1:2))+diadic(nvec(1:2),nvec(1:2) )+diadic(tvec(1:2),mvec(1:2) )&
			-diadic(nvec(1:2),ovec(1:2))-diadic(ovec(1:2),nvec(1:2) ) )/sjk!!+-
		dmmat(1:2,7:8)=0.0d0
		
		dnmat__(1:2,1:2)=HH*ganma_hat/sjk*diadic(tvec__(1:2),tvec(1:2) )
		dnmat__(1:2,3:4)=-HH*ganma_hat/sjk*diadic(tvec__(1:2),mvec(1:2))
		dnmat__(1:2,5:6)=HH*ganma_hat/sjk*diadic(tvec__(1:2),mvec(1:2)-tvec(1:2) )!!+-
		dnmat__(1:2,7:8)=0.0d0
		
		dnmat__(1:2,1:2)=dnmat__(1:2,1:2)+0.0d0
		dnmat__(1:2,3:4)=dnmat__(1:2,3:4)+1.0d0/sel*alpha*diadic(tvec__(1:2),nvec_(1:2) ) 
		dnmat__(1:2,5:6)=dnmat__(1:2,5:6)-1.0d0/sel*diadic(tvec__(1:2),nvec_(1:2) )
		dnmat__(1:2,7:8)=dnmat__(1:2,7:8)+1.0d0/sel*(1.0d0-alpha)*diadic(tvec__(1:2),nvec_(1:2) )
		 
		dgzivec(1:2)=1.0d0/sjk*tvec(1:2)
		dgzivec(3:4)=-1.0d0/sjk*mvec(1:2)
		dgzivec(5:6)=1.0d0/sjk*( mvec(1:2)-tvec(1:2) )!!+-
		dgzivec(7:8)=0.0d0
		
		dalpha(1:2)=HH/sjk*tvec(1:2)
		dalpha(3:4)=-HH/sjk*mvec(1:2)
		dalpha(5:6)=HH/sjk*( mvec(1:2)-tvec(1:2) )!!+-
		dalpha(7:8)=0.0d0
		
		dHvec(1:2)=kappa/sjk*tvec(1:2)
		dHvec(3:4)=-kappa/sjk*mvec(1:2)
		dHvec(5:6)=kappa/sjk*( mvec(1:2)-tvec(1:2) )!!+-
		dHvec(7:8)=0.0d0
		
		dgzivec_(1:2)=HH*(gzi_*gzi_hat-ganma_*ganma_hat)/sjk*tvec(1:2)
		dgzivec_(3:4)=-HH*(gzi_*gzi_hat-ganma_*ganma_hat)/sjk*mvec(1:2)
		dgzivec_(5:6)=HH*(gzi_*gzi_hat-ganma_*ganma_hat)/sjk*(mvec(1:2)-tvec(1:2) )!!+-
		dgzivec_(7:8)=0.0d0
		
		dgzivec_(1:2)=dgzivec_(1:2)+1.0d0/sel*tvec_(1:2)
		dgzivec_(3:4)=dgzivec_(3:4)+1.0d0/sel*(alpha*mvec_(1:2)-tvec_(1:2)) 
		dgzivec_(5:6)=dgzivec_(5:6)+1.0d0/sel*(-1.0d0)*mvec_(1:2)
		dgzivec_(7:8)=dgzivec_(7:8)+1.0d0/sel*(1.0d0-alpha)*mvec_(1:2)
		
		Dns(1:8,1:8)=diadic(Dns_1_1,domega_mat)
		
		
		Dns(1:2,1:8)=Dns(1:2,1:8)+omega*dtmat(1:2,1:8)
		Dns(3:4,1:8)=Dns(3:4,1:8)+omega*(-dmmat(1:2,1:8) )
		Dns(5:6,1:8)=Dns(5:6,1:8)+omega*( dmmat(1:2,1:8)-dtmat(1:2,1:8) )!!+-
		Dns(7:8,1:8)=Dns(7:8,1:8)+0.0d0
		
		Dns(1:2,1:8)=Dns(1:2,1:8)+dnmat__(1:2,1:8)
		Dns(3:4,1:8)=Dns(3:4,1:8)+diadic(nvec__(1:2),alpha*dgzivec_(1:8)+gzi_*dalpha(1:8) )-(1.0d0-alpha*gzi_)*dnmat__(1:2,1:8) 
		Dns(5:6,1:8)=Dns(5:6,1:8)-diadic(nvec__(1:2),dgzivec_(1:8) )-gzi_*dnmat__(1:2,1:8)
		Dns(7:8,1:8)=Dns(7:8,1:8)+diadic(nvec__(1:2),(1.0d0-alpha)*dgzivec_(1:8)-gzi_*dalpha(1:8) )+(1.0d0-alpha)*gzi_*dnmat__(1:2,1:8)
		
		
		fvec_e(1:8)= en*gns_*ns(1:8)
		K_st(1:8,1:8)=en*(diadic(ns,ns)+gns_*Dns(1:8,1:8) )
		! note >> du(1),du(2),du(3),du(4)
		
		!tangential part>>>
		
	
		dnmat_(1:2,1:2)=HH*ganma_hat/sjk*diadic(tvec_(1:2),tvec(1:2) )
		dnmat_(1:2,3:4)=-HH*ganma_hat/sjk*diadic(tvec_(1:2),mvec(1:2))
		dnmat_(1:2,5:6)=HH*ganma_hat/sjk*diadic(tvec_(1:2),mvec(1:2)-tvec(1:2) )!!+-
		dnmat_(1:2,7:8)=0.0d0
		
		dnmat_(1:2,1:2)=dnmat__(1:2,1:2)+0.0d0
		dnmat_(1:2,3:4)=dnmat__(1:2,3:4)+1.0d0/sel*alpha*diadic(tvec_(1:2),nvec_(1:2) ) 
		dnmat_(1:2,5:6)=dnmat__(1:2,5:6)-1.0d0/sel*diadic(tvec_(1:2),nvec_(1:2) )
		dnmat_(1:2,7:8)=dnmat__(1:2,7:8)+1.0d0/sel*(1.0d0-alpha)*diadic(tvec_(1:2),nvec_(1:2) )
		 
		
		dganmavec_(1:2)=HH*(gzi_*ganma_hat+gzi_hat*ganma_)/sjk*(tvec(1:2))
		dganmavec_(3:4)=HH*(gzi_*ganma_hat+gzi_hat*ganma_)/sjk*(-mvec(1:2))
		dganmavec_(5:6)=HH*(gzi_*ganma_hat+gzi_hat*ganma_)/sjk*(mvec(1:2)-tvec(1:2))!!+-
		dganmavec_(7:8)=0.0d0
		
		dganmavec_(1:2)=dganmavec_(1:2)+1.0d0/sel*(nvec_(1:2))
		dganmavec_(3:4)=dganmavec_(3:4)+1.0d0/sel*(alpha*(gzi_*nvec_(1:2)+ganma_*tvec_(1:2))-nvec_(1:2) ) 
		dganmavec_(5:6)=dganmavec_(5:6)+1.0d0/sel*(-(gzi_*nvec_(1:2)+ganma_*tvec_(1:2) ))
		dganmavec_(7:8)=dganmavec_(7:8)+1.0d0/sel*((1.0d0-alpha)*(gzi_*nvec_(1:2)+ganma_*tvec_(1:2)))
		
		dganma_hat_vec(1:2)=2.0d0*HH*gzi_hat*ganma_hat/sjk*(tvec(1:2))
		dganma_hat_vec(3:4)=2.0d0*HH*gzi_hat*ganma_hat/sjk*(-mvec(1:2))
		dganma_hat_vec(5:6)=2.0d0*HH*gzi_hat*ganma_hat/sjk*(mvec(1:2)-tvec(1:2))!!+-
		dganma_hat_vec(7:8)=0.0d0
		
		
		dganma_hat_vec(1:2)=dganma_hat_vec(1:2)+0.0d0
		dganma_hat_vec(3:4)=dganma_hat_vec(3:4)+1.0d0/sel*(alpha*(gzi_hat*nvec_(1:2)+ganma_hat*tvec_(1:2))+nvec_(1:2) ) 
		dganma_hat_vec(5:6)=dganma_hat_vec(5:6)+1.0d0/sel*(-(gzi_hat*nvec_(1:2)+ganma_hat*tvec_(1:2) ))
		dganma_hat_vec(7:8)=dganma_hat_vec(7:8)+1.0d0/sel*((1.0d0-alpha)*(gzi_hat*nvec_(1:2)+ganma_hat*tvec_(1:2))-nvec_(1:2) )
		
		dgzi_hat_vec(1:2)=HH*(gzi_hat*gzi_hat-ganma_hat*ganma_hat)/sjk*(tvec(1:2))
		dgzi_hat_vec(3:4)=HH*(gzi_hat*gzi_hat-ganma_hat*ganma_hat)/sjk*(-mvec(1:2))
		dgzi_hat_vec(5:6)=HH*(gzi_hat*gzi_hat-ganma_hat*ganma_hat)/sjk*(mvec(1:2)-tvec(1:2))!!+-
		dgzi_hat_vec(7:8)=0.0d0
		
		dgzi_hat_vec(1:2)=dgzi_hat_vec(1:2)+0.0d0
		dgzi_hat_vec(3:4)=dgzi_hat_vec(3:4)+1.0d0/sel*((gzi_hat*tvec_(1:2)-ganma_hat*nvec_(1:2))*alpha+tvec_(1:2) )
		dgzi_hat_vec(5:6)=dgzi_hat_vec(5:6)+1.0d0/sel*(-(gzi_hat*tvec_(1:2)-ganma_hat*nvec_(1:2)) ) 
		dgzi_hat_vec(7:8)=dgzi_hat_vec(7:8)+1.0d0/sel*((1.0d0-alpha)*(gzi_hat*tvec_(1:2)-ganma_hat*nvec_(1:2))-tvec_(1:2) )
		
		
		
		
		dtmat_(1:2,1:2)=HH*ganma_hat/sjk*(-1.0d0)*(diadic( nvec_(1:2),tvec(1:2) ) )
		dtmat_(1:2,3:4)=HH*ganma_hat/sjk*(1.0d0)*(diadic( nvec_(1:2),mvec(1:2) ) )
		dtmat_(1:2,5:6)=HH*ganma_hat/sjk*(-1.0d0)*(diadic( nvec_(1:2),mvec(1:2)-tvec(1:2) ) )!!+-
		dtmat_(1:2,7:8)=0.0d0
	
		dtmat_(1:2,1:2)=dtmat_(1:2,1:2)+0.0d0
		dtmat_(1:2,3:4)=dtmat_(1:2,3:4)+1.0d0/sel*(-1.0d0)*alpha*diadic( nvec_(1:2), nvec_(1:2) ) 
		dtmat_(1:2,5:6)=dtmat_(1:2,5:6)+1.0d0/sel*(1.0d0)*diadic( nvec_(1:2), nvec_(1:2) )
		dtmat_(1:2,7:8)=dtmat_(1:2,7:8)+1.0d0/sel*(-1.0d0+alpha)*diadic( nvec_(1:2), nvec_(1:2) )
		
		
		
		
		dmmat_(1:2,1:8)=diadic(tvec_(1:2),dgzivec_(1:8) )
		
		dmmat_(1:2,1:8)=dmmat_(1:2,1:8)+gzi_*dtmat_(1:2,1:8)
		
		dmmat_(1:2,1:8)=dmmat_(1:2,1:8)+diadic( nvec_(1:2), dganmavec_(1:8))
		
		dmmat_(1:2,1:8)=dmmat_(1:2,1:8)+ganma_*dnmat_(1:2,1:8)
		
		dselvec(1:2)=sel*HH*gzi_hat/sjk*(-1.0d0)*tvec(1:2)
		dselvec(3:4)=sel*HH*gzi_hat/sjk*(-1.0d0)*(mvec(1:2)-tvec(1:2))
		dselvec(5:6)=sel*HH*gzi_hat/sjk*mvec(1:2)
		dselvec(7:8)=0.0d0
		
		dselvec(1:2)=dselvec(1:2)+0.0d0
		dselvec(3:4)=dselvec(3:4)+tvec_(1:2)
		dselvec(5:6)=dselvec(5:6)+(-alpha)*tvec_(1:2) !!+-
		dselvec(7:8)=dselvec(7:8)-(1.0d0-alpha)*tvec_(1:2)
		
		dlamdavec_(1:8)=gzi_*dgzi_hat_vec(1:8)+gzi_hat*dgzivec_(1:8)&
			-ganma_hat*dganmavec_(1:8)-ganma_*dganma_hat_vec(1:8)
		
		!original part
		dsjkvec(1:2)=dble(beta)*0.0d0
		dsjkvec(3:4)=dble(beta)*(-1.0d0)*tvec(1:2)
		dsjkvec(5:6)=dble(beta)*tvec(1:2)
		dsjkvec(7:8)=dble(beta)*0.0d0
		
		lamda_=gzi_*gzi_hat-ganma_*ganma_hat
		T0=1.0d0/sjk*HH*lamda_
		
		dT0vec(1:8)=-HH*lamda_/sjk/sjk*dsjkvec(1:8)+HH/sjk*dlamdavec_(1:8)+lamda_/sjk*dHvec(1:8)
		
		Svec(1:2)=-sel*HH*gzi_hat/sjk*tvec(1:2)
		Svec(3:4)=-sel*HH*gzi_hat/sjk*(-mvec(1:2))
		Svec(5:6)=-sel*HH*gzi_hat/sjk*mvec(1:2)-tvec(1:2)!!+-
		Svec(7:8)=0.0d0
		
		Svec(1:2)=Svec(1:2)+0.0d0
		Svec(3:4)=Svec(3:4)+(-alpha)*tvec_(1:2)  
		Svec(5:6)=Svec(5:6)+tvec_(1:2)
		Svec(7:8)=Svec(7:8)-(1.0d0-alpha)*tvec_(1:2)
		
		nt(1:2)=T0*tvec(1:2)
		nt(3:4)=T0*(-mvec(1:2) )
		nt(5:6)=T0*( mvec(1:2)-tvec(1:2)     )!!+-
		nt(7:8)=0.0d0		
			
		nt(1:2)=nt(1:2)+1.0d0/sel*tvec_(1:2)
		nt(3:4)=nt(3:4)+1.0d0/sel*( alpha*mvec_(1:2)-tvec_(1:2)  ) 
		nt(5:6)=nt(5:6)+1.0d0/sel*(-mvec_(1:2))
		nt(7:8)=nt(7:8)+1.0d0/sel*(1.0d0-alpha )*mvec_(1:2)		

		
		Dnt(1:2,1:8)=diadic(tvec(1:2),dT0vec(1:8) )+T0*dtmat(1:2,1:8)
		Dnt(3:4,1:8)=-diadic(mvec(1:2),dT0vec(1:8) )-T0*dmmat(1:2,1:8)
		Dnt(5:6,1:8)=diadic(mvec(1:2)-tvec(1:2),dT0vec(1:8) )+T0*(dmmat(1:2,1:8)  -dtmat(1:2,1:8))!!+-
		Dnt(7:8,1:8)=0.0d0
		
		Dnt(1:2,1:8)=Dnt(1:2,1:8)-1.0d0/sel/sel*diadic(tvec_(1:2),dselvec(1:8) )
		Dnt(3:4,1:8)=Dnt(3:4,1:8)-1.0d0/sel/sel*diadic( alpha*mvec_(1:2)- tvec_(1:2),dselvec(1:8) ) !inverse original
		Dnt(5:6,1:8)=Dnt(5:6,1:8)-1.0d0/sel/sel*diadic(-mvec_(1:2),dselvec(1:8) )
		Dnt(7:8,1:8)=Dnt(7:8,1:8)-1.0d0/sel/sel*diadic( (1.0d0-alpha)*mvec_(1:2),dselvec(1:8) )
		
		Dnt(1:2,1:8)=Dnt(1:2,1:8)+1.0d0/sel*dtmat_(1:2,1:8)
		Dnt(3:4,1:8)=Dnt(3:4,1:8)+1.0d0/sel*(diadic(mvec_(1:2),dalpha(1:8) )+alpha*dmmat_(1:2,1:8)-dtmat_(1:2,1:8))!inverse original
		Dnt(5:6,1:8)=Dnt(5:6,1:8)+1.0d0/sel*(-dmmat_(1:2,1:8) )
		Dnt(7:8,1:8)=Dnt(7:8,1:8)+1.0d0/sel*(-diadic(mvec_(1:2),dalpha(1:8) )+(1.0d0-alpha)*dmmat_(1:2,1:8))
		
		
		if(stick_slip( active_nts(j)  )==0  )then
			Ft(1:8)=dble(beta)*ct*sel*nt(1:8)
		elseif(stick_slip( active_nts(j)  )==1  )then
			Ft(1:8)=en*tan(phy)*ns(1:8)
		else
			 stop  "invalid stick_slip on contact.f95"
		endif
		fvec_e(1:8)= fvec_e(1:8)+dble(beta)*tts*sel*nt(1:8)
		K_st(1:8,1:8)=K_st(1:8,1:8)+dble(beta)*transpose( sel*diadic(Ft(1:8),nt(1:8))+tts*diadic(Svec(1:8),nt(1:8))+tts*sel*Dnt(1:8,1:8))
		fvec_e(:)=fvec_e(:)*l !integration
		K_st(:,:)=K_st(:,:)*l !integration

	
		do i = 1,4
			do ii = 1, 4
				if(i==3)then
					i_1=4
				elseif(i==4)then
					i_1=3
				else
					i_1=i
				endif
				
				if(ii==3)then
					ii_1=4
				elseif(ii==4)then
					ii_1=3
				else
					ii_1=ii
				endif
				
				k_contact(2*nts_elem_nod(active_nts(j),i_1)-1,2*nts_elem_nod(active_nts(j),ii_1)-1) &
				=k_contact(2*nts_elem_nod(active_nts(j),i_1)-1,2*nts_elem_nod(active_nts(j),ii_1)-1) &
				+k_st(2*i_1-1,2*ii_1-1)
				k_contact(2*nts_elem_nod(active_nts(j),i_1)-1,2*nts_elem_nod(active_nts(j),ii_1)) &
				=k_contact(2*nts_elem_nod(active_nts(j),i_1)-1,2*nts_elem_nod(active_nts(j),ii_1)) &
				+k_st(2*i_1-1,2*ii_1)
				k_contact(2*nts_elem_nod(active_nts(j),i_1),2*nts_elem_nod(active_nts(j),ii_1)-1) &
				=k_contact(2*nts_elem_nod(active_nts(j),i_1),2*nts_elem_nod(active_nts(j),ii_1)-1) &
				+k_st(2*i_1,2*ii_1-1)
				k_contact(2*nts_elem_nod(active_nts(j),i_1),2*nts_elem_nod(active_nts(j),ii_1)) &
				=k_contact(2*nts_elem_nod(active_nts(j),i_1),2*nts_elem_nod(active_nts(j),ii_1)) &
				+k_st(2*i_1,2*ii_1)
			
			enddo
		enddo
		
		do i=1,4
			if(i==3)then
				i_1=4
			elseif(i==4)then
				i_1=3
			else
				i_1=i
			endif
			
			
			fvec_contact(2*nts_elem_nod(active_nts(j),i_1)-1 ) &
				=fvec_contact(2*nts_elem_nod(active_nts(j),i_1)-1 )+fvec_e(2*i_1-1)
			fvec_contact(2*nts_elem_nod(active_nts(j),i_1)) &
				=fvec_contact(2*nts_elem_nod(active_nts(j),i_1))+fvec_e(2*i_1)	
		enddo		
			
			
		else
		
			 stop  "error :: invalid beta"
		endif
		
			do k=1,size(fvec_contact)
				if(fvec_contact(k)>=0.0d0 .or. fvec_contact(k)<0.0d0 )then
					cycle
				else
					 stop  "NaN ct !!"
				endif
			enddo

	 !諸量の更新
	 nts_amo(active_nts(j),1)     =gz0 !trial gzi0 on current timestep
	 !nts_amo(active_nts(j),10)    =gz !converged gzi at last timestep
	 nts_amo(active_nts(j),2)     =dble(beta) !trial beta on current timestep
	 !nts_amo(active_nts(j),11)    =pn !inactive


	 
	 
	  
 end subroutine update_res_grad_c_i

!==============================================================
 subroutine update_res_grad_c(j,nod_max,old_nod_coord,nts_elem_nod,active_nts&
              ,nts_amo, k_contact,uvec,duvec,fvec_contact,stick_slip,contact_mat_para,nts_mat)
			  
	real(real64), allocatable ::x2s(:),x11(:),x12(:),evec(:),avec(:),nvec(:)&
	,k_st(:,:),ns(:),n0s(:),ts(:),ts_st(:),t0s(:),ngz0(:),fvec_e(:),nod_coord(:,:),&
	nvec_(:),tvec_(:),x1(:),x2(:),x3(:),x4(:),x5(:),x6(:),tvec(:),mvec(:),yi(:),Dns(:,:),&
	ym(:),ys(:),nvec__(:),ovec(:),mvec_(:),mvec__(:),Dns_1(:),Dns_2(:),Dns_3(:),domega_mat(:),&
	Dns_1_1(:),Ivec(:),dtmat(:,:),dmmat(:,:),dnmat__(:,:),dgzivec(:),dalpha(:),dHvec(:),nt(:),&
	Dnt(:,:),dT0vec(:),dtmat_(:,:),dselvec(:),dmmat_(:,:),dgzi_hat_vec(:),dganma_hat_vec(:),&
	dganmavec_(:),dnmat_(:,:),dgzivec_(:),dsjkvec(:),dlamdavec_(:),Svec(:),Ft(:),yL(:),tvec__(:),&
	ye(:),yj(:),yk(:),c_nod_coord(:,:)
	

	real(real64) ,intent(inout)::nts_amo(:,:),k_contact(:,:),fvec_contact(:)
	real(real64), intent(in) :: old_nod_coord(:,:),uvec(:),contact_mat_para(:,:),duvec(:)
    integer, intent(in) :: j, nod_max,nts_elem_nod(:,:),active_nts(:),nts_mat(:)
	integer, intent(inout) :: stick_slip(:)
    real(real64) c,phy,en,ct,gns,gz,l,pn,tts,gt,gz0,alpha,omega,gns_,gz_,sjk,delta
	real(real64) gzi_hat,delta_hat,ganma_,kappa,S0,ganma,gzi_,ganma_hat,lamda_,T0,dfdtn,HH,sel
	integer i, ii , k,beta,i_1,ii_1,node_ID
	 
	 ! 0 duvecの格納,ξ,ｇN等諸量の格納
	 allocate(x2s(2),x11(2),x12(2),evec(3),avec(3),nvec(3),k_st(8,8),&
	 ns(8),n0s(8),ts(8),t0s(8),ngz0(8),ts_st(8),fvec_e(8),tvec(2),mvec(2) )
	 allocate( nvec_(3),tvec_(3),x1(2),x2(2),x3(2),x4(2),x5(2),x6(2),yi(2) )
	 allocate(ym(2),ys(2),nvec__(2),ovec(2),mvec_(2),mvec__(2),Dns(8,8),Dns_1_1(8) )
	 allocate(Dns_1(8),domega_mat(8),Ivec(2) )
	allocate(dtmat(2,8),dmmat(2,8),dnmat__(2,8),dgzivec(8),dalpha(8),dHvec(8) )
	allocate(nod_coord(size(old_nod_coord,1),size(old_nod_coord,2)))
	allocate(nt(8),Dnt(8,8),dT0vec(8),dtmat_(2,8),dselvec(8),dmmat_(2,8),dgzi_hat_vec(8)  )
	allocate( dganma_hat_vec(8),dganmavec_(8),dnmat_(2,8),dgzivec_(8),dsjkvec(8),dlamdavec_(8)  )
	allocate(Svec(8),Ft(8),yL(2),tvec__(1:2) )
	allocate(ye(2),yj(2),yk(2),c_nod_coord(size(nod_coord,1),size(nod_coord,2)  ) )
	do i=1, size(nod_coord,1)
		nod_coord(i,1)=old_nod_coord(i,1)
		nod_coord(i,2)=old_nod_coord(i,2)
	enddo
	do i=1,size(nod_coord,1)
		c_nod_coord(i,1)=nod_coord(i,1)+uvec(2*i-1)
		c_nod_coord(i,2)=nod_coord(i,2)+uvec(2*i  )
	 enddo 
	 !-----材料パラメータの読み込み------
	 en=contact_mat_para(nts_mat( active_nts(j) ),2 )
	 ct=contact_mat_para(nts_mat( active_nts(j) ),1 )
	 c = contact_mat_para(nts_mat( active_nts(j) ),3 )
	 phy=contact_mat_para(nts_mat( active_nts(j) ),4 )
	 !--------------------------------
	  delta=1.0e-5
	 tts=nts_amo(active_nts(j),12) 
	 !dfdtn=nts_amo(active_nts(j),10) 
	 if(tts>=0.0d0)then
		dfdtn=1.0d0
	  elseif(tts<0.0d0)then
		dfdtn=-1.0d0
	 else
		 stop "invalid tTs"
	  endif
	 
	 
	 !以下、初期座標＋変位により、位置ベクトルを更新し、諸量を更新
	 !gz更新
	 x1(1:2) = uvec(2*nts_elem_nod(active_nts(j),1)-1:&
	    2*nts_elem_nod(active_nts(j),1))+&
		duvec(2*nts_elem_nod(active_nts(j),1)-1:&
	    2*nts_elem_nod(active_nts(j),1))+&
		nod_coord(nts_elem_nod(active_nts(j),1),1:2)
	 x2(1:2) = uvec(2*nts_elem_nod(active_nts(j),2)-1:&
	    2*nts_elem_nod(active_nts(j),2))+&
		duvec(2*nts_elem_nod(active_nts(j),2)-1:&
	    2*nts_elem_nod(active_nts(j),2))+&
		nod_coord(nts_elem_nod(active_nts(j),2),1:2)
	 x3(1:2) = uvec(2*nts_elem_nod(active_nts(j),3)-1:&
	    2*nts_elem_nod(active_nts(j),3))+&
		duvec(2*nts_elem_nod(active_nts(j),3)-1:&
	    2*nts_elem_nod(active_nts(j),3))+&
		nod_coord(nts_elem_nod(active_nts(j),3),1:2)	 
	 x4(1:2) = uvec(2*nts_elem_nod(active_nts(j),4)-1:&
	    2*nts_elem_nod(active_nts(j),4))+&
		duvec(2*nts_elem_nod(active_nts(j),4)-1:&
	    2*nts_elem_nod(active_nts(j),4))+&
		nod_coord(nts_elem_nod(active_nts(j),4),1:2)
	x5(1:2) = uvec(2*nts_elem_nod(active_nts(j),5)-1:&
	    2*nts_elem_nod(active_nts(j),5))+&
		duvec(2*nts_elem_nod(active_nts(j),5)-1:&
	    2*nts_elem_nod(active_nts(j),5))+&
		nod_coord(nts_elem_nod(active_nts(j),5),1:2)
	x6(1:2) = uvec(2*nts_elem_nod(active_nts(j),6)-1:&
	    2*nts_elem_nod(active_nts(j),6))+&
		duvec(2*nts_elem_nod(active_nts(j),6)-1:&
	    2*nts_elem_nod(active_nts(j),6))+&
		nod_coord(nts_elem_nod(active_nts(j),6),1:2)
	 node_ID=active_nts(j)
	 
	 
	call get_beta_st_nts(node_ID,nts_elem_nod,c_nod_coord,beta)
	if(beta==1)then
		x2s(1:2) = x1(:)
		x11(1:2) = x2(:)
		x12(1:2) = x3(:)
		yi(1:2) = x4(:)
		yj(1:2) = x2(1:2)
		yk(1:2) = x3(1:2)
		ys(1:2) = x1(1:2)
		ym(1:2) = x2(1:2)
		ye(1:2) = x3(1:2)
		
		
		
	else
		x2s(1:2) = x1(:)
		x11(1:2) = x4(:)
		x12(1:2) = x2(:)
		yi(1:2) = x3(:)
		yj(1:2) = x4(1:2)
		yk(1:2) = x2(1:2)
		ys(1:2) = x1(1:2)
		ym(1:2) = x2(1:2)
		ye(1:2) = x4(1:2)
		
	endif
		 ! 0 duvecの格納,ξ,ｇN等諸量の格納
	 !-----------------------------------------------------------------------
	!-----------------------------------------------------------------------

	 nvec(3) = 0.0d0
	 
	 avec(3) = 0.0d0
	 
	 evec(1) = 0.0d0
	 evec(2) = 0.0d0
	 evec(3) = 1.0d0
	 
	 Ivec(1) = 1.0d0
	 Ivec(2) = 1.0d0
	 
	 nvec_(3) = 0.0d0
	 tvec_(3) = 0.0d0
	!----------------------------------
	 l = dot_product( yj(1:2)-yk(1:2), yj(1:2)-yk(1:2)) 
	 l=dsqrt(l)
	sjk=l
	 if(l==0.0d0)then
		print *, "l=0 at element No.",node_ID
		 stop 
	 endif
	
	avec(1:2) = ( yk(1:2)-yj(1:2)  )/l

	 nvec(:) = cross_product(evec,avec)
	 gz=1.0d0/l*dot_product(ys(1:2)-yj(1:2),avec(1:2) )
	 gns = dot_product((ys(:)-ym(:)),nvec(1:2))
	 
	 

	 !alpha=4.0d0*gz*(1.0d0-gz)
	 !alpha=0.50d0*(1.0d0-cos(2.0d0*3.1415926535d0*gz) )
	 !alpha=exp( -delta*delta*(2.0d0*gz-1.0d0)**2.0d0 )
	 alpha=1.0d0
	 !alpha=0.0d0
	 yL(:)=yi(:)+alpha*(ym(:)-yi(:))
	 sel=dsqrt(dot_product(ye-yL,ye-yL))
	 gz0=gz-tts/ct/sel

	 if(sel==0.0d0)then
			 stop  "error check_gn"
	endif
	tvec_(1:2)=(ye(:)-yL(:) )/sel
	nvec_(:)=cross_product(evec,tvec_)
	tvec(1:2)=avec(1:2)
	mvec(:)=gz*tvec(:)-gns/sjk*nvec(:)
	nvec__(1:2)=nvec_(1:2)*dble(beta) 
	 
	 !gnsの計算と更新-----------------------------------------------------
	 gns_ = dot_product((ys(:)-ym(:)),nvec__(1:2))	 
	 gz_=1.0d0/sel*dot_product(ys-ym,tvec_(1:2) )
	 
	 !get f_contact(normal),K_contact(normal)
	 !compute common variables
	 gzi_=1.0d0/sel*dot_product(ys-ym,tvec_(1:2)  )
	 ganma_hat=1.0d0/sel*dot_product(ym-yi,nvec_(1:2) )
	 !HH=4.0d0*(1.0d0-2.0d0*gz)
	 !HH=-3.1415926535d0*cos(2.0d0*3.1415926535d0*gz)
	 HH=alpha*(delta*delta)*(4.0d0-8.0d0*gz)
	 HH=0.0d0
	 
	 omega=1.0d0/sjk*HH*gz_*dot_product(ym-yi,nvec__(1:2) )
	 
	 gzi_hat=1.0d0/sel*dot_product(ym-yi,tvec_(1:2) )
	 delta_hat=dot_product(ym-yi,nvec_(1:2) )
	 ganma_=1.0d0/sel*dot_product(ys-ym,nvec_(1:2) )
	
	 ganma=gns/sjk
	 ovec(1:2)=gz*nvec(1:2)+ganma*tvec(1:2)
	 mvec_(1:2)=gzi_*tvec_(1:2)-ganma_*nvec_(1:2)
	 mvec__(1:2)=gzi_*tvec_(1:2)-ganma_*nvec_(1:2)
	 !kappa=-8.0d0
	 !kappa=-2.0d0*3.1415926535d0*3.1415926535d0*cos(2.0d0*3.1415926535d0*gz)
	 kappa=alpha*(delta)*(delta)*(delta)*(delta)*(4.0d0-8.0d0*gz) - 8.0d0*alpha*(delta*delta)
	 kappa=0.0d0
	 !kappa=8.0d0
	 tvec__(1:2)=dble(beta)*tvec_(1:2)
	 S0=delta_hat*dble(beta)/sjk*( kappa*gzi_+HH*HH*(2.0d0*gzi_*gzi_hat-ganma_*ganma_hat)  )
	 
	 if(beta==1)then
		!normal part >>>
		ns(1:2)=omega*(tvec(1:2)  )
		ns(3:4)=omega*(mvec(1:2)-tvec(1:2))!!+-
		ns(5:6)=omega*(-mvec(1:2)  )
		ns(7:8)=0.0d0
	 
		ns(1:2)=ns(1:2)+nvec__(1:2)
		ns(3:4)=ns(3:4)-(1.0d0-alpha*gz_)*nvec__(1:2) 
		ns(5:6)=ns(5:6)-gz_*nvec__(1:2)
		ns(7:8)=ns(7:8)+(1.0d0- alpha)*gz_*nvec__(1:2)
		
		Dns_1_1(1:2)=tvec(1:2)
		Dns_1_1(3:4)=mvec(1:2)-tvec(1:2)
		Dns_1_1(5:6)=-mvec(:)
		Dns_1_1(7:8)=0.0d0
		
		domega_mat(1:2)=1.0d0/sjk*S0*tvec(:)
		domega_mat(3:4)=1.0d0/sjk*(S0*(mvec(1:2)-tvec(1:2))+omega*tvec(1:2))!!+-
		domega_mat(5:6)=1.0d0/sjk*(-S0*mvec(1:2)-omega*tvec(1:2))
		domega_mat(7:8)=0.0d0
		
		domega_mat(1:2)=domega_mat(1:2)+HH/sjk*ganma_hat*tvec__(1:2)
		domega_mat(3:4)=domega_mat(3:4)+HH/sjk*( ganma_hat*(alpha*mvec__(1:2)-tvec__(1:2) )+gzi_*(1.0d0+alpha*gzi_hat)*nvec__(1:2))
		domega_mat(5:6)=domega_mat(5:6)+HH/sjk*(-ganma_hat*mvec__(1:2)-gzi_*ganma_hat*nvec__(1:2) ) 
		domega_mat(7:8)=domega_mat(7:8)+HH/sjk*(gzi_*(-1.0d0+(1.0d0-alpha )*gzi_hat)*nvec__(1:2)+ganma_hat*(1.0d0-alpha)*mvec__(1:2)  )
		
		dtmat(1:2,1:2)=0.0d0
		dtmat(1:2,3:4)=-diadic(nvec(1:2),nvec(1:2) )/sjk!!+-
		dtmat(1:2,5:6)=diadic(nvec(1:2),nvec(1:2) )/sjk
		dtmat(1:2,7:8)=0.0d0
		
		dmmat(1:2,1:2)=(diadic(tvec(1:2),tvec(1:2))-diadic(nvec(1:2),nvec(1:2) ))/sjk!!+-
		dmmat(1:2,3:4)=(-diadic(tvec(1:2),tvec(1:2))+diadic(nvec(1:2),nvec(1:2) )+diadic(tvec(1:2),mvec(1:2) )&
			-diadic(nvec(1:2),ovec(1:2))-diadic(ovec(1:2),nvec(1:2) ) )/sjk
		dmmat(1:2,5:6)=(-diadic(tvec(1:2),mvec(1:2) )&
			+diadic(nvec(1:2),ovec(1:2))+diadic(ovec(1:2),nvec(1:2) ) )/sjk
		dmmat(1:2,7:8)=0.0d0
		
		dnmat__(1:2,1:2)=HH*ganma_hat/sjk*diadic(tvec__(1:2),tvec(1:2) )
		dnmat__(1:2,3:4)=HH*ganma_hat/sjk*diadic(tvec__(1:2),mvec(1:2)-tvec(1:2) )!!+-
		dnmat__(1:2,5:6)=-HH*ganma_hat/sjk*diadic(tvec__(1:2),mvec(1:2))
		dnmat__(1:2,7:8)=0.0d0
		
		dnmat__(1:2,1:2)=dnmat__(1:2,1:2)+0.0d0
		dnmat__(1:2,3:4)=dnmat__(1:2,3:4)+1.0d0/sel*alpha*diadic(tvec__(1:2),nvec_(1:2) ) 
		dnmat__(1:2,5:6)=dnmat__(1:2,5:6)-1.0d0/sel*diadic(tvec__(1:2),nvec_(1:2) )
		dnmat__(1:2,7:8)=dnmat__(1:2,7:8)+1.0d0/sel*(1.0d0-alpha)*diadic(tvec__(1:2),nvec_(1:2) )
		 
		dgzivec(1:2)=1.0d0/sjk*tvec(1:2)
		dgzivec(3:4)=1.0d0/sjk*( mvec(1:2)-tvec(1:2) )!!+-
		dgzivec(5:6)=-1.0d0/sjk*mvec(1:2)
		dgzivec(7:8)=0.0d0
		
		dalpha(1:2)=HH/sjk*tvec(1:2)
		dalpha(3:4)=HH/sjk*( mvec(1:2)-tvec(1:2) )!!+-
		dalpha(5:6)=-HH/sjk*mvec(1:2)
		dalpha(7:8)=0.0d0
		
		dHvec(1:2)=kappa/sjk*tvec(1:2)
		dHvec(3:4)=kappa/sjk*( mvec(1:2)-tvec(1:2) )!!+-
		dHvec(5:6)=-kappa/sjk*mvec(1:2)
		dHvec(7:8)=0.0d0
		
		dgzivec_(1:2)=HH*(gzi_*gzi_hat-ganma_*ganma_hat)/sjk*tvec(1:2)
		dgzivec_(3:4)=HH*(gzi_*gzi_hat-ganma_*ganma_hat)/sjk*(mvec(1:2)-tvec(1:2) )!!+-
		dgzivec_(5:6)=-HH*(gzi_*gzi_hat-ganma_*ganma_hat)/sjk*mvec(1:2)
		dgzivec_(7:8)=0.0d0
		
		dgzivec_(1:2)=dgzivec_(1:2)+1.0d0/sel*tvec_(1:2)
		dgzivec_(3:4)=dgzivec_(3:4)+1.0d0/sel*(alpha*mvec_(1:2)-tvec_(1:2)) 
		dgzivec_(5:6)=dgzivec_(5:6)+1.0d0/sel*(-1.0d0)*mvec_(1:2)
		dgzivec_(7:8)=dgzivec_(7:8)+1.0d0/sel*(1.0d0-alpha)*mvec_(1:2)
		
		Dns(1:8,1:8)=diadic(Dns_1_1,domega_mat)
		
		
		Dns(1:2,1:8)=Dns(1:2,1:8)+omega*dtmat(1:2,1:8)
		Dns(3:4,1:8)=Dns(3:4,1:8)+omega*( dmmat(1:2,1:8)-dtmat(1:2,1:8) )!!+-
		Dns(5:6,1:8)=Dns(5:6,1:8)+omega*(-dmmat(1:2,1:8) )
		Dns(7:8,1:8)=Dns(7:8,1:8)+0.0d0
		
		Dns(1:2,1:8)=Dns(1:2,1:8)+dnmat__(1:2,1:8)
		Dns(3:4,1:8)=Dns(3:4,1:8)+diadic(nvec__(1:2),alpha*dgzivec_(1:8)+gzi_*dalpha(1:8) )-(1.0d0-alpha*gzi_)*dnmat__(1:2,1:8) 
		Dns(5:6,1:8)=Dns(5:6,1:8)-diadic(nvec__(1:2),dgzivec_(1:8) )-gzi_*dnmat__(1:2,1:8)
		Dns(7:8,1:8)=Dns(7:8,1:8)+diadic(nvec__(1:2),(1.0d0-alpha)*dgzivec_(1:8)-gzi_*dalpha(1:8) )+(1.0d0-alpha)*gzi_*dnmat__(1:2,1:8)
		
		
		fvec_e(1:8)= en*gns_*ns(1:8)
		K_st(1:8,1:8)=en*(diadic(ns,ns)+gns_*Dns(1:8,1:8) )
		! note >> du(1),du(2),du(3),du(4)
		
		!tangential part>>>
		
	
		dnmat_(1:2,1:2)=HH*ganma_hat/sjk*diadic(tvec_(1:2),tvec(1:2) )
		dnmat_(1:2,3:4)=HH*ganma_hat/sjk*diadic(tvec_(1:2),mvec(1:2)-tvec(1:2) )!!+-
		dnmat_(1:2,5:6)=-HH*ganma_hat/sjk*diadic(tvec_(1:2),mvec(1:2))
		dnmat_(1:2,7:8)=0.0d0
		
		dnmat_(1:2,1:2)=dnmat__(1:2,1:2)+0.0d0
		dnmat_(1:2,3:4)=dnmat__(1:2,3:4)+1.0d0/sel*alpha*diadic(tvec_(1:2),nvec_(1:2) ) 
		dnmat_(1:2,5:6)=dnmat__(1:2,5:6)-1.0d0/sel*diadic(tvec_(1:2),nvec_(1:2) )
		dnmat_(1:2,7:8)=dnmat__(1:2,7:8)+1.0d0/sel*(1.0d0-alpha)*diadic(tvec_(1:2),nvec_(1:2) )
		 
		
		dganmavec_(1:2)=HH*(gzi_*ganma_hat+gzi_hat*ganma_)/sjk*(tvec(1:2))
		dganmavec_(3:4)=HH*(gzi_*ganma_hat+gzi_hat*ganma_)/sjk*(mvec(1:2)-tvec(1:2))!!+-
		dganmavec_(5:6)=HH*(gzi_*ganma_hat+gzi_hat*ganma_)/sjk*(-mvec(1:2))
		dganmavec_(7:8)=0.0d0
		
		dganmavec_(1:2)=dganmavec_(1:2)+1.0d0/sel*(nvec_(1:2))
		dganmavec_(3:4)=dganmavec_(3:4)+1.0d0/sel*(alpha*(gzi_*nvec_(1:2)+ganma_*tvec_(1:2))-nvec_(1:2) ) 
		dganmavec_(5:6)=dganmavec_(5:6)+1.0d0/sel*(-(gzi_*nvec_(1:2)+ganma_*tvec_(1:2) ))
		dganmavec_(7:8)=dganmavec_(7:8)+1.0d0/sel*((1.0d0-alpha)*(gzi_*nvec_(1:2)+ganma_*tvec_(1:2)))
		
		dganma_hat_vec(1:2)=2.0d0*HH*gzi_hat*ganma_hat/sjk*(tvec(1:2))
		dganma_hat_vec(3:4)=2.0d0*HH*gzi_hat*ganma_hat/sjk*(mvec(1:2)-tvec(1:2))!!+-
		dganma_hat_vec(5:6)=2.0d0*HH*gzi_hat*ganma_hat/sjk*(-mvec(1:2))
		dganma_hat_vec(7:8)=0.0d0
		
		
		dganma_hat_vec(1:2)=dganma_hat_vec(1:2)+0.0d0
		dganma_hat_vec(3:4)=dganma_hat_vec(3:4)+1.0d0/sel*(alpha*(gzi_hat*nvec_(1:2)+ganma_hat*tvec_(1:2))+nvec_(1:2) ) 
		dganma_hat_vec(5:6)=dganma_hat_vec(5:6)+1.0d0/sel*(-(gzi_hat*nvec_(1:2)+ganma_hat*tvec_(1:2) ))
		dganma_hat_vec(7:8)=dganma_hat_vec(7:8)+1.0d0/sel*((1.0d0-alpha)*(gzi_hat*nvec_(1:2)+ganma_hat*tvec_(1:2))-nvec_(1:2) )
		
		dgzi_hat_vec(1:2)=HH*(gzi_hat*gzi_hat-ganma_hat*ganma_hat)/sjk*(tvec(1:2))
		dgzi_hat_vec(3:4)=HH*(gzi_hat*gzi_hat-ganma_hat*ganma_hat)/sjk*(mvec(1:2)-tvec(1:2))!!+-
		dgzi_hat_vec(5:6)=HH*(gzi_hat*gzi_hat-ganma_hat*ganma_hat)/sjk*(-mvec(1:2))
		dgzi_hat_vec(7:8)=0.0d0
		
		dgzi_hat_vec(1:2)=dgzi_hat_vec(1:2)+0.0d0
		dgzi_hat_vec(3:4)=dgzi_hat_vec(3:4)+1.0d0/sel*((gzi_hat*tvec_(1:2)-ganma_hat*nvec_(1:2))*alpha+tvec_(1:2) )
		dgzi_hat_vec(5:6)=dgzi_hat_vec(5:6)+1.0d0/sel*(-(gzi_hat*tvec_(1:2)-ganma_hat*nvec_(1:2)) ) 
		dgzi_hat_vec(7:8)=dgzi_hat_vec(7:8)+1.0d0/sel*((1.0d0-alpha)*(gzi_hat*tvec_(1:2)-ganma_hat*nvec_(1:2))-tvec_(1:2) )
		
		
		
		
		dtmat_(1:2,1:2)=HH*ganma_hat/sjk*(-1.0d0)*(diadic( nvec_(1:2),tvec(1:2) ) )
		dtmat_(1:2,3:4)=HH*ganma_hat/sjk*(-1.0d0)*(diadic( nvec_(1:2),mvec(1:2)-tvec(1:2) ) )!!+-
		dtmat_(1:2,5:6)=HH*ganma_hat/sjk*(1.0d0)*(diadic( nvec_(1:2),mvec(1:2) ) )
		dtmat_(1:2,7:8)=0.0d0
	
		dtmat_(1:2,1:2)=dtmat_(1:2,1:2)+0.0d0
		dtmat_(1:2,3:4)=dtmat_(1:2,3:4)+1.0d0/sel*(-1.0d0)*alpha*diadic( nvec_(1:2), nvec_(1:2) ) 
		dtmat_(1:2,5:6)=dtmat_(1:2,5:6)+1.0d0/sel*(1.0d0)*diadic( nvec_(1:2), nvec_(1:2) )
		dtmat_(1:2,7:8)=dtmat_(1:2,7:8)+1.0d0/sel*(-1.0d0+alpha)*diadic( nvec_(1:2), nvec_(1:2) )
		
		
		
		
		dmmat_(1:2,1:8)=diadic(tvec_(1:2),dgzivec_(1:8) )
		
		dmmat_(1:2,1:8)=dmmat_(1:2,1:8)+gzi_*dtmat_(1:2,1:8)
		
		dmmat_(1:2,1:8)=dmmat_(1:2,1:8)+diadic( nvec_(1:2), dganmavec_(1:8))
		
		dmmat_(1:2,1:8)=dmmat_(1:2,1:8)+ganma_*dnmat_(1:2,1:8)
		
		dselvec(1:2)=sel*HH*gzi_hat/sjk*(-1.0d0)*tvec(1:2)
		dselvec(3:4)=sel*HH*gzi_hat/sjk*(-1.0d0)*(mvec(1:2)-tvec(1:2))
		dselvec(5:6)=sel*HH*gzi_hat/sjk*mvec(1:2)
		dselvec(7:8)=0.0d0
		
		dselvec(1:2)=dselvec(1:2)+0.0d0
		dselvec(3:4)=dselvec(3:4)+(-alpha)*tvec_(1:2) !!+-
		dselvec(5:6)=dselvec(5:6)+tvec_(1:2)
		dselvec(7:8)=dselvec(7:8)-(1.0d0-alpha)*tvec_(1:2)
		
		dlamdavec_(1:8)=gzi_*dgzi_hat_vec(1:8)+gzi_hat*dgzivec_(1:8)&
			-ganma_hat*dganmavec_(1:8)-ganma_*dganma_hat_vec(1:8)
		
		!original part
		dsjkvec(1:2)=dble(beta)*0.0d0
		dsjkvec(3:4)=dble(beta)*(-1.0d0)*tvec(1:2)
		dsjkvec(5:6)=dble(beta)*tvec(1:2)
		dsjkvec(7:8)=dble(beta)*0.0d0
		
		lamda_=gzi_*gzi_hat-ganma_*ganma_hat
		T0=1.0d0/sjk*HH*lamda_
		
		dT0vec(1:8)=-HH*lamda_/sjk/sjk*dsjkvec(1:8)+HH/sjk*dlamdavec_(1:8)+lamda_/sjk*dHvec(1:8)
		
		Svec(1:2)=-sel*HH*gzi_hat/sjk*tvec(1:2)
		Svec(3:4)=-sel*HH*gzi_hat/sjk*mvec(1:2)-tvec(1:2)!!+-
		Svec(5:6)=-sel*HH*gzi_hat/sjk*(-mvec(1:2))
		Svec(7:8)=0.0d0
		
		Svec(1:2)=Svec(1:2)+0.0d0
		Svec(3:4)=Svec(3:4)+(-alpha)*tvec_(1:2)  
		Svec(5:6)=Svec(5:6)+tvec_(1:2)
		Svec(7:8)=Svec(7:8)-(1.0d0-alpha)*tvec_(1:2)
		
		nt(1:2)=T0*tvec(1:2)
		nt(3:4)=T0*( mvec(1:2)-tvec(1:2)     )!!+-
		nt(5:6)=T0*(-mvec(1:2) )
		nt(7:8)=0.0d0		
			
		nt(1:2)=nt(1:2)+1.0d0/sel*tvec_(1:2)
		nt(3:4)=nt(3:4)+1.0d0/sel*( alpha*mvec_(1:2)-tvec_(1:2)  ) 
		nt(5:6)=nt(5:6)+1.0d0/sel*(-mvec_(1:2))
		nt(7:8)=nt(7:8)+1.0d0/sel*(1.0d0-alpha )*mvec_(1:2)		

		
		Dnt(1:2,1:8)=diadic(tvec(1:2),dT0vec(1:8) )+T0*dtmat(1:2,1:8)
		Dnt(3:4,1:8)=diadic(mvec(1:2)-tvec(1:2),dT0vec(1:8) )+T0*(dmmat(1:2,1:8)  -dtmat(1:2,1:8))!!+-
		Dnt(5:6,1:8)=-diadic(mvec(1:2),dT0vec(1:8) )-T0*dmmat(1:2,1:8)
		Dnt(7:8,1:8)=0.0d0
		
		Dnt(1:2,1:8)=Dnt(1:2,1:8)-1.0d0/sel/sel*diadic(tvec_(1:2),dselvec(1:8) )
		Dnt(3:4,1:8)=Dnt(3:4,1:8)-1.0d0/sel/sel*diadic( alpha*mvec_(1:2)- tvec_(1:2),dselvec(1:8) ) !inverse original
		Dnt(5:6,1:8)=Dnt(5:6,1:8)-1.0d0/sel/sel*diadic(-mvec_(1:2),dselvec(1:8) )
		Dnt(7:8,1:8)=Dnt(7:8,1:8)-1.0d0/sel/sel*diadic( (1.0d0-alpha)*mvec_(1:2),dselvec(1:8) )
		
		Dnt(1:2,1:8)=Dnt(1:2,1:8)+1.0d0/sel*dtmat_(1:2,1:8)
		Dnt(3:4,1:8)=Dnt(3:4,1:8)+1.0d0/sel*(diadic(mvec_(1:2),dalpha(1:8) )+alpha*dmmat_(1:2,1:8)-dtmat_(1:2,1:8))!inverse original
		Dnt(5:6,1:8)=Dnt(5:6,1:8)+1.0d0/sel*(-dmmat_(1:2,1:8) )
		Dnt(7:8,1:8)=Dnt(7:8,1:8)+1.0d0/sel*(-diadic(mvec_(1:2),dalpha(1:8) )+(1.0d0-alpha)*dmmat_(1:2,1:8))
		
		
		if(stick_slip( active_nts(j)  )==0  )then
			Ft(1:8)=dble(beta)*ct*sel*nt(1:8)
		elseif(stick_slip( active_nts(j)  )==1  )then
			Ft(1:8)=en*tan(phy)*ns(1:8)
		else
			 stop  "invalid stick_slip on contact.f95"
		endif
		fvec_e(1:8)= fvec_e(1:8)+dble(beta)*tts*sel*nt(1:8)
		K_st(1:8,1:8)=K_st(1:8,1:8)+dble(beta)*transpose( sel*diadic(Ft(1:8),nt(1:8))+tts*diadic(Svec(1:8),nt(1:8))+tts*sel*Dnt(1:8,1:8))
		fvec_e(:)=fvec_e(:)*l !integration
		K_st(:,:)=K_st(:,:)*l !integration
		do i = 1,4
			do ii = 1, 4
			
			k_contact(2*nts_elem_nod(active_nts(j),i)-1,2*nts_elem_nod(active_nts(j),ii)-1) &
			=k_contact(2*nts_elem_nod(active_nts(j),i)-1,2*nts_elem_nod(active_nts(j),ii)-1) &
			+k_st(2*i-1,2*ii-1)
			k_contact(2*nts_elem_nod(active_nts(j),i)-1,2*nts_elem_nod(active_nts(j),ii)) &
			=k_contact(2*nts_elem_nod(active_nts(j),i)-1,2*nts_elem_nod(active_nts(j),ii)) &
			+k_st(2*i-1,2*ii)
			k_contact(2*nts_elem_nod(active_nts(j),i),2*nts_elem_nod(active_nts(j),ii)-1) &
			=k_contact(2*nts_elem_nod(active_nts(j),i),2*nts_elem_nod(active_nts(j),ii)-1) &
			+k_st(2*i,2*ii-1)
			k_contact(2*nts_elem_nod(active_nts(j),i),2*nts_elem_nod(active_nts(j),ii)) &
			=k_contact(2*nts_elem_nod(active_nts(j),i),2*nts_elem_nod(active_nts(j),ii)) &
			+k_st(2*i,2*ii)
		
			enddo
		enddo
		
		do i=1,4
			fvec_contact(2*nts_elem_nod(active_nts(j),i)-1 ) &
				=fvec_contact(2*nts_elem_nod(active_nts(j),i)-1 )+fvec_e(2*i-1)
			fvec_contact(2*nts_elem_nod(active_nts(j),i)) &
				=fvec_contact(2*nts_elem_nod(active_nts(j),i))+fvec_e(2*i)	
		enddo
	

	

		
	 elseif(beta==-1)then
		!normal part >>>
		!normal part >>>
		ns(1:2)=omega*(tvec(1:2)  )
		ns(3:4)=omega*(-mvec(1:2)  )
		ns(5:6)=omega*(mvec(1:2)-tvec(1:2))!!+-
		ns(7:8)=0.0d0
	 
		ns(1:2)=ns(1:2)+nvec__(1:2)
		ns(3:4)=ns(3:4)-(1.0d0-alpha*gz_)*nvec__(1:2) 
		ns(5:6)=ns(5:6)-gz_*nvec__(1:2)
		ns(7:8)=ns(7:8)+(1.0d0- alpha)*gz_*nvec__(1:2)
		
		Dns_1_1(1:2)=tvec(1:2)
		Dns_1_1(3:4)=-mvec(:)
		Dns_1_1(5:6)=mvec(1:2)-tvec(1:2)!!+-
		Dns_1_1(7:8)=0.0d0
		
		domega_mat(1:2)=1.0d0/sjk*S0*tvec(:)
		domega_mat(3:4)=1.0d0/sjk*(-S0*mvec(1:2)-omega*tvec(1:2))
		domega_mat(5:6)=1.0d0/sjk*(S0*(mvec(1:2)-tvec(1:2))+omega*tvec(1:2))!!+-
		domega_mat(7:8)=0.0d0
		
		domega_mat(1:2)=domega_mat(1:2)+HH/sjk*ganma_hat*tvec__(1:2)
		domega_mat(3:4)=domega_mat(3:4)+HH/sjk*( ganma_hat*(alpha*mvec__(1:2)-tvec__(1:2) )+gzi_*(1.0d0+alpha*gzi_hat)*nvec__(1:2))
		domega_mat(5:6)=domega_mat(5:6)+HH/sjk*(-ganma_hat*mvec__(1:2)-gzi_*ganma_hat*nvec__(1:2) ) 
		domega_mat(7:8)=domega_mat(7:8)+HH/sjk*(gzi_*(-1.0d0+(1.0d0-alpha )*gzi_hat)*nvec__(1:2)+ganma_hat*(1.0d0-alpha)*mvec__(1:2)  )
		
		dtmat(1:2,1:2)=0.0d0
		dtmat(1:2,3:4)=diadic(nvec(1:2),nvec(1:2) )/sjk
		dtmat(1:2,5:6)=-diadic(nvec(1:2),nvec(1:2) )/sjk!!+-
		dtmat(1:2,7:8)=0.0d0
		
		dmmat(1:2,1:2)=(diadic(tvec(1:2),tvec(1:2))-diadic(nvec(1:2),nvec(1:2) ))/sjk
		dmmat(1:2,3:4)=(-diadic(tvec(1:2),mvec(1:2) )&
			+diadic(nvec(1:2),ovec(1:2))+diadic(ovec(1:2),nvec(1:2) ) )/sjk
		dmmat(1:2,5:6)=(-diadic(tvec(1:2),tvec(1:2))+diadic(nvec(1:2),nvec(1:2) )+diadic(tvec(1:2),mvec(1:2) )&
			-diadic(nvec(1:2),ovec(1:2))-diadic(ovec(1:2),nvec(1:2) ) )/sjk!!+-
		dmmat(1:2,7:8)=0.0d0
		
		dnmat__(1:2,1:2)=HH*ganma_hat/sjk*diadic(tvec__(1:2),tvec(1:2) )
		dnmat__(1:2,3:4)=-HH*ganma_hat/sjk*diadic(tvec__(1:2),mvec(1:2))
		dnmat__(1:2,5:6)=HH*ganma_hat/sjk*diadic(tvec__(1:2),mvec(1:2)-tvec(1:2) )!!+-
		dnmat__(1:2,7:8)=0.0d0
		
		dnmat__(1:2,1:2)=dnmat__(1:2,1:2)+0.0d0
		dnmat__(1:2,3:4)=dnmat__(1:2,3:4)+1.0d0/sel*alpha*diadic(tvec__(1:2),nvec_(1:2) ) 
		dnmat__(1:2,5:6)=dnmat__(1:2,5:6)-1.0d0/sel*diadic(tvec__(1:2),nvec_(1:2) )
		dnmat__(1:2,7:8)=dnmat__(1:2,7:8)+1.0d0/sel*(1.0d0-alpha)*diadic(tvec__(1:2),nvec_(1:2) )
		 
		dgzivec(1:2)=1.0d0/sjk*tvec(1:2)
		dgzivec(3:4)=-1.0d0/sjk*mvec(1:2)
		dgzivec(5:6)=1.0d0/sjk*( mvec(1:2)-tvec(1:2) )!!+-
		dgzivec(7:8)=0.0d0
		
		dalpha(1:2)=HH/sjk*tvec(1:2)
		dalpha(3:4)=-HH/sjk*mvec(1:2)
		dalpha(5:6)=HH/sjk*( mvec(1:2)-tvec(1:2) )!!+-
		dalpha(7:8)=0.0d0
		
		dHvec(1:2)=kappa/sjk*tvec(1:2)
		dHvec(3:4)=-kappa/sjk*mvec(1:2)
		dHvec(5:6)=kappa/sjk*( mvec(1:2)-tvec(1:2) )!!+-
		dHvec(7:8)=0.0d0
		
		dgzivec_(1:2)=HH*(gzi_*gzi_hat-ganma_*ganma_hat)/sjk*tvec(1:2)
		dgzivec_(3:4)=-HH*(gzi_*gzi_hat-ganma_*ganma_hat)/sjk*mvec(1:2)
		dgzivec_(5:6)=HH*(gzi_*gzi_hat-ganma_*ganma_hat)/sjk*(mvec(1:2)-tvec(1:2) )!!+-
		dgzivec_(7:8)=0.0d0
		
		dgzivec_(1:2)=dgzivec_(1:2)+1.0d0/sel*tvec_(1:2)
		dgzivec_(3:4)=dgzivec_(3:4)+1.0d0/sel*(alpha*mvec_(1:2)-tvec_(1:2)) 
		dgzivec_(5:6)=dgzivec_(5:6)+1.0d0/sel*(-1.0d0)*mvec_(1:2)
		dgzivec_(7:8)=dgzivec_(7:8)+1.0d0/sel*(1.0d0-alpha)*mvec_(1:2)
		
		Dns(1:8,1:8)=diadic(Dns_1_1,domega_mat)
		
		
		Dns(1:2,1:8)=Dns(1:2,1:8)+omega*dtmat(1:2,1:8)
		Dns(3:4,1:8)=Dns(3:4,1:8)+omega*(-dmmat(1:2,1:8) )
		Dns(5:6,1:8)=Dns(5:6,1:8)+omega*( dmmat(1:2,1:8)-dtmat(1:2,1:8) )!!+-
		Dns(7:8,1:8)=Dns(7:8,1:8)+0.0d0
		
		Dns(1:2,1:8)=Dns(1:2,1:8)+dnmat__(1:2,1:8)
		Dns(3:4,1:8)=Dns(3:4,1:8)+diadic(nvec__(1:2),alpha*dgzivec_(1:8)+gzi_*dalpha(1:8) )-(1.0d0-alpha*gzi_)*dnmat__(1:2,1:8) 
		Dns(5:6,1:8)=Dns(5:6,1:8)-diadic(nvec__(1:2),dgzivec_(1:8) )-gzi_*dnmat__(1:2,1:8)
		Dns(7:8,1:8)=Dns(7:8,1:8)+diadic(nvec__(1:2),(1.0d0-alpha)*dgzivec_(1:8)-gzi_*dalpha(1:8) )+(1.0d0-alpha)*gzi_*dnmat__(1:2,1:8)
		
		
		fvec_e(1:8)= en*gns_*ns(1:8)
		K_st(1:8,1:8)=en*(diadic(ns,ns)+gns_*Dns(1:8,1:8) )
		! note >> du(1),du(2),du(3),du(4)
		
		!tangential part>>>
		
	
		dnmat_(1:2,1:2)=HH*ganma_hat/sjk*diadic(tvec_(1:2),tvec(1:2) )
		dnmat_(1:2,3:4)=-HH*ganma_hat/sjk*diadic(tvec_(1:2),mvec(1:2))
		dnmat_(1:2,5:6)=HH*ganma_hat/sjk*diadic(tvec_(1:2),mvec(1:2)-tvec(1:2) )!!+-
		dnmat_(1:2,7:8)=0.0d0
		
		dnmat_(1:2,1:2)=dnmat__(1:2,1:2)+0.0d0
		dnmat_(1:2,3:4)=dnmat__(1:2,3:4)+1.0d0/sel*alpha*diadic(tvec_(1:2),nvec_(1:2) ) 
		dnmat_(1:2,5:6)=dnmat__(1:2,5:6)-1.0d0/sel*diadic(tvec_(1:2),nvec_(1:2) )
		dnmat_(1:2,7:8)=dnmat__(1:2,7:8)+1.0d0/sel*(1.0d0-alpha)*diadic(tvec_(1:2),nvec_(1:2) )
		 
		
		dganmavec_(1:2)=HH*(gzi_*ganma_hat+gzi_hat*ganma_)/sjk*(tvec(1:2))
		dganmavec_(3:4)=HH*(gzi_*ganma_hat+gzi_hat*ganma_)/sjk*(-mvec(1:2))
		dganmavec_(5:6)=HH*(gzi_*ganma_hat+gzi_hat*ganma_)/sjk*(mvec(1:2)-tvec(1:2))!!+-
		dganmavec_(7:8)=0.0d0
		
		dganmavec_(1:2)=dganmavec_(1:2)+1.0d0/sel*(nvec_(1:2))
		dganmavec_(3:4)=dganmavec_(3:4)+1.0d0/sel*(alpha*(gzi_*nvec_(1:2)+ganma_*tvec_(1:2))-nvec_(1:2) ) 
		dganmavec_(5:6)=dganmavec_(5:6)+1.0d0/sel*(-(gzi_*nvec_(1:2)+ganma_*tvec_(1:2) ))
		dganmavec_(7:8)=dganmavec_(7:8)+1.0d0/sel*((1.0d0-alpha)*(gzi_*nvec_(1:2)+ganma_*tvec_(1:2)))
		
		dganma_hat_vec(1:2)=2.0d0*HH*gzi_hat*ganma_hat/sjk*(tvec(1:2))
		dganma_hat_vec(3:4)=2.0d0*HH*gzi_hat*ganma_hat/sjk*(-mvec(1:2))
		dganma_hat_vec(5:6)=2.0d0*HH*gzi_hat*ganma_hat/sjk*(mvec(1:2)-tvec(1:2))!!+-
		dganma_hat_vec(7:8)=0.0d0
		
		
		dganma_hat_vec(1:2)=dganma_hat_vec(1:2)+0.0d0
		dganma_hat_vec(3:4)=dganma_hat_vec(3:4)+1.0d0/sel*(alpha*(gzi_hat*nvec_(1:2)+ganma_hat*tvec_(1:2))+nvec_(1:2) ) 
		dganma_hat_vec(5:6)=dganma_hat_vec(5:6)+1.0d0/sel*(-(gzi_hat*nvec_(1:2)+ganma_hat*tvec_(1:2) ))
		dganma_hat_vec(7:8)=dganma_hat_vec(7:8)+1.0d0/sel*((1.0d0-alpha)*(gzi_hat*nvec_(1:2)+ganma_hat*tvec_(1:2))-nvec_(1:2) )
		
		dgzi_hat_vec(1:2)=HH*(gzi_hat*gzi_hat-ganma_hat*ganma_hat)/sjk*(tvec(1:2))
		dgzi_hat_vec(3:4)=HH*(gzi_hat*gzi_hat-ganma_hat*ganma_hat)/sjk*(-mvec(1:2))
		dgzi_hat_vec(5:6)=HH*(gzi_hat*gzi_hat-ganma_hat*ganma_hat)/sjk*(mvec(1:2)-tvec(1:2))!!+-
		dgzi_hat_vec(7:8)=0.0d0
		
		dgzi_hat_vec(1:2)=dgzi_hat_vec(1:2)+0.0d0
		dgzi_hat_vec(3:4)=dgzi_hat_vec(3:4)+1.0d0/sel*((gzi_hat*tvec_(1:2)-ganma_hat*nvec_(1:2))*alpha+tvec_(1:2) )
		dgzi_hat_vec(5:6)=dgzi_hat_vec(5:6)+1.0d0/sel*(-(gzi_hat*tvec_(1:2)-ganma_hat*nvec_(1:2)) ) 
		dgzi_hat_vec(7:8)=dgzi_hat_vec(7:8)+1.0d0/sel*((1.0d0-alpha)*(gzi_hat*tvec_(1:2)-ganma_hat*nvec_(1:2))-tvec_(1:2) )
		
		
		
		
		dtmat_(1:2,1:2)=HH*ganma_hat/sjk*(-1.0d0)*(diadic( nvec_(1:2),tvec(1:2) ) )
		dtmat_(1:2,3:4)=HH*ganma_hat/sjk*(1.0d0)*(diadic( nvec_(1:2),mvec(1:2) ) )
		dtmat_(1:2,5:6)=HH*ganma_hat/sjk*(-1.0d0)*(diadic( nvec_(1:2),mvec(1:2)-tvec(1:2) ) )!!+-
		dtmat_(1:2,7:8)=0.0d0
	
		dtmat_(1:2,1:2)=dtmat_(1:2,1:2)+0.0d0
		dtmat_(1:2,3:4)=dtmat_(1:2,3:4)+1.0d0/sel*(-1.0d0)*alpha*diadic( nvec_(1:2), nvec_(1:2) ) 
		dtmat_(1:2,5:6)=dtmat_(1:2,5:6)+1.0d0/sel*(1.0d0)*diadic( nvec_(1:2), nvec_(1:2) )
		dtmat_(1:2,7:8)=dtmat_(1:2,7:8)+1.0d0/sel*(-1.0d0+alpha)*diadic( nvec_(1:2), nvec_(1:2) )
		
		
		
		
		dmmat_(1:2,1:8)=diadic(tvec_(1:2),dgzivec_(1:8) )
		
		dmmat_(1:2,1:8)=dmmat_(1:2,1:8)+gzi_*dtmat_(1:2,1:8)
		
		dmmat_(1:2,1:8)=dmmat_(1:2,1:8)+diadic( nvec_(1:2), dganmavec_(1:8))
		
		dmmat_(1:2,1:8)=dmmat_(1:2,1:8)+ganma_*dnmat_(1:2,1:8)
		
		dselvec(1:2)=sel*HH*gzi_hat/sjk*(-1.0d0)*tvec(1:2)
		dselvec(3:4)=sel*HH*gzi_hat/sjk*(-1.0d0)*(mvec(1:2)-tvec(1:2))
		dselvec(5:6)=sel*HH*gzi_hat/sjk*mvec(1:2)
		dselvec(7:8)=0.0d0
		
		dselvec(1:2)=dselvec(1:2)+0.0d0
		dselvec(3:4)=dselvec(3:4)+tvec_(1:2)
		dselvec(5:6)=dselvec(5:6)+(-alpha)*tvec_(1:2) !!+-
		dselvec(7:8)=dselvec(7:8)-(1.0d0-alpha)*tvec_(1:2)
		
		dlamdavec_(1:8)=gzi_*dgzi_hat_vec(1:8)+gzi_hat*dgzivec_(1:8)&
			-ganma_hat*dganmavec_(1:8)-ganma_*dganma_hat_vec(1:8)
		
		!original part
		dsjkvec(1:2)=dble(beta)*0.0d0
		dsjkvec(3:4)=dble(beta)*(-1.0d0)*tvec(1:2)
		dsjkvec(5:6)=dble(beta)*tvec(1:2)
		dsjkvec(7:8)=dble(beta)*0.0d0
		
		lamda_=gzi_*gzi_hat-ganma_*ganma_hat
		T0=1.0d0/sjk*HH*lamda_
		
		dT0vec(1:8)=-HH*lamda_/sjk/sjk*dsjkvec(1:8)+HH/sjk*dlamdavec_(1:8)+lamda_/sjk*dHvec(1:8)
		
		Svec(1:2)=-sel*HH*gzi_hat/sjk*tvec(1:2)
		Svec(3:4)=-sel*HH*gzi_hat/sjk*(-mvec(1:2))
		Svec(5:6)=-sel*HH*gzi_hat/sjk*mvec(1:2)-tvec(1:2)!!+-
		Svec(7:8)=0.0d0
		
		Svec(1:2)=Svec(1:2)+0.0d0
		Svec(3:4)=Svec(3:4)+(-alpha)*tvec_(1:2)  
		Svec(5:6)=Svec(5:6)+tvec_(1:2)
		Svec(7:8)=Svec(7:8)-(1.0d0-alpha)*tvec_(1:2)
		
		nt(1:2)=T0*tvec(1:2)
		nt(3:4)=T0*(-mvec(1:2) )
		nt(5:6)=T0*( mvec(1:2)-tvec(1:2)     )!!+-
		nt(7:8)=0.0d0		
			
		nt(1:2)=nt(1:2)+1.0d0/sel*tvec_(1:2)
		nt(3:4)=nt(3:4)+1.0d0/sel*( alpha*mvec_(1:2)-tvec_(1:2)  ) 
		nt(5:6)=nt(5:6)+1.0d0/sel*(-mvec_(1:2))
		nt(7:8)=nt(7:8)+1.0d0/sel*(1.0d0-alpha )*mvec_(1:2)		

		
		Dnt(1:2,1:8)=diadic(tvec(1:2),dT0vec(1:8) )+T0*dtmat(1:2,1:8)
		Dnt(3:4,1:8)=-diadic(mvec(1:2),dT0vec(1:8) )-T0*dmmat(1:2,1:8)
		Dnt(5:6,1:8)=diadic(mvec(1:2)-tvec(1:2),dT0vec(1:8) )+T0*(dmmat(1:2,1:8)  -dtmat(1:2,1:8))!!+-
		Dnt(7:8,1:8)=0.0d0
		
		Dnt(1:2,1:8)=Dnt(1:2,1:8)-1.0d0/sel/sel*diadic(tvec_(1:2),dselvec(1:8) )
		Dnt(3:4,1:8)=Dnt(3:4,1:8)-1.0d0/sel/sel*diadic( alpha*mvec_(1:2)- tvec_(1:2),dselvec(1:8) ) !inverse original
		Dnt(5:6,1:8)=Dnt(5:6,1:8)-1.0d0/sel/sel*diadic(-mvec_(1:2),dselvec(1:8) )
		Dnt(7:8,1:8)=Dnt(7:8,1:8)-1.0d0/sel/sel*diadic( (1.0d0-alpha)*mvec_(1:2),dselvec(1:8) )
		
		Dnt(1:2,1:8)=Dnt(1:2,1:8)+1.0d0/sel*dtmat_(1:2,1:8)
		Dnt(3:4,1:8)=Dnt(3:4,1:8)+1.0d0/sel*(diadic(mvec_(1:2),dalpha(1:8) )+alpha*dmmat_(1:2,1:8)-dtmat_(1:2,1:8))!inverse original
		Dnt(5:6,1:8)=Dnt(5:6,1:8)+1.0d0/sel*(-dmmat_(1:2,1:8) )
		Dnt(7:8,1:8)=Dnt(7:8,1:8)+1.0d0/sel*(-diadic(mvec_(1:2),dalpha(1:8) )+(1.0d0-alpha)*dmmat_(1:2,1:8))
		
		
		if(stick_slip( active_nts(j)  )==0  )then
			Ft(1:8)=dble(beta)*ct*sel*nt(1:8)
		elseif(stick_slip( active_nts(j)  )==1  )then
			Ft(1:8)=en*tan(phy)*ns(1:8)
		else
			 stop  "invalid stick_slip on contact.f95"
		endif
		fvec_e(1:8)= fvec_e(1:8)+dble(beta)*tts*sel*nt(1:8)
		K_st(1:8,1:8)=K_st(1:8,1:8)+dble(beta)*transpose( sel*diadic(Ft(1:8),nt(1:8))+tts*diadic(Svec(1:8),nt(1:8))+tts*sel*Dnt(1:8,1:8))

		fvec_e(:)=fvec_e(:)*l !integration
		K_st(:,:)=K_st(:,:)*l !integration	
		do i = 1,4
			do ii = 1, 4
				if(i==3)then
					i_1=4
				elseif(i==4)then
					i_1=3
				else
					i_1=i
				endif
				
				if(ii==3)then
					ii_1=4
				elseif(ii==4)then
					ii_1=3
				else
					ii_1=ii
				endif
				
				k_contact(2*nts_elem_nod(active_nts(j),i_1)-1,2*nts_elem_nod(active_nts(j),ii_1)-1) &
				=k_contact(2*nts_elem_nod(active_nts(j),i_1)-1,2*nts_elem_nod(active_nts(j),ii_1)-1) &
				+k_st(2*i_1-1,2*ii_1-1)
				k_contact(2*nts_elem_nod(active_nts(j),i_1)-1,2*nts_elem_nod(active_nts(j),ii_1)) &
				=k_contact(2*nts_elem_nod(active_nts(j),i_1)-1,2*nts_elem_nod(active_nts(j),ii_1)) &
				+k_st(2*i_1-1,2*ii_1)
				k_contact(2*nts_elem_nod(active_nts(j),i_1),2*nts_elem_nod(active_nts(j),ii_1)-1) &
				=k_contact(2*nts_elem_nod(active_nts(j),i_1),2*nts_elem_nod(active_nts(j),ii_1)-1) &
				+k_st(2*i_1,2*ii_1-1)
				k_contact(2*nts_elem_nod(active_nts(j),i_1),2*nts_elem_nod(active_nts(j),ii_1)) &
				=k_contact(2*nts_elem_nod(active_nts(j),i_1),2*nts_elem_nod(active_nts(j),ii_1)) &
				+k_st(2*i_1,2*ii_1)
			
			enddo
		enddo
		
		do i=1,4
			if(i==3)then
				i_1=4
			elseif(i==4)then
				i_1=3
			else
				i_1=i
			endif
			
			
			fvec_contact(2*nts_elem_nod(active_nts(j),i_1)-1 ) &
				=fvec_contact(2*nts_elem_nod(active_nts(j),i_1)-1 )+fvec_e(2*i_1-1)
			fvec_contact(2*nts_elem_nod(active_nts(j),i_1)) &
				=fvec_contact(2*nts_elem_nod(active_nts(j),i_1))+fvec_e(2*i_1)	
		enddo		
			
			
		else
		
			 stop  "error :: invalid beta"
		endif


	
	 !諸量の更新
	 !nts_amo(active_nts(j),1)     =gz !trial gzi0 on current timestep
	 !nts_amo(active_nts(j),10)    =gz !converged gzi at last timestep
	 !nts_amo(active_nts(j),11)    =pn !inactive

 end subroutine update_res_grad_c

!==============================================================
! 変位を与えた節点でRvec=0.0d0とする
!-----------------------------------

 subroutine disp_rvec(u_nod_x,u_nod_y,rvec)
       integer, intent(in) :: u_nod_x(:),u_nod_y(:)
	   real(real64), intent(inout) :: rvec(:)
	   integer i
	   
	   !x方向変位を設定した残差ベクトル成分を0.0d0
	    do i=1,size(u_nod_x,1)    
			rvec(2*u_nod_x(i)-1)=0.0d0
        enddo
		
		!y方向変位を設定した残差ベクトル成分を0.0d0
	    do i=1,size(u_nod_y,1)    
			rvec(2*u_nod_y(i))=0.0d0
        enddo

 end subroutine disp_rvec
!===============================================================
 subroutine get_beta_st_nts(nts_ID,nts_elem_nod,nod_coord,beta)
	integer,intent(in)::nts_ID,nts_elem_nod(:,:)
	integer,intent(out)::beta
	integer i,j,n
	real(real64),intent(in)::nod_coord(:,:)
	real(real64),allocatable::tvec_0(:),x1(:),x2(:),x3(:),a(:)
	real(real64) direction

	n=size(nod_coord,2)
	
	allocate(tvec_0(n),x1(n),x2(n),x3(n),a(n) )
	
	
	
	x1(:)=nod_coord(nts_elem_nod(nts_ID,2),:)
	x2(:)=nod_coord(nts_elem_nod(nts_ID,3),:)
	x3(:)=nod_coord(nts_elem_nod(nts_ID,4),:)
	a(:)=nod_coord(nts_elem_nod(nts_ID,1),:)-x1(:)
	
	tvec_0(:)=x2(:)-x3(:)
	tvec_0(:)=tvec_0(:)/dsqrt(dot_product(tvec_0,tvec_0))
	
	direction=dot_product(a,tvec_0)
	
	if(direction<=0.0d0)then
		beta=-1
	elseif(direction>0.0d0)then
		beta=1
	else
		!print *, dsqrt(dot_product(tvec_0,tvec_0)),a(1:2),size(a)
		 stop  "ERROR on get_beta_st_nts, contact.mod"
	endif
	
 end subroutine get_beta_st_nts
!===============================================================

subroutine ls_nts_generateCM(obj)
	! 配列の宣言
  	class(ContactMechanics_),intent(inout) :: obj
 
  	integer,allocatable:: mast_slav(:,:),mast_slav_es(:,:),&
  	nts_elem_nod_es(:,:),master_nod(:),master_nod_es(:),slave_nod(:),&
  	slave_nod_es(:)
  
  

  	real(real64), allocatable ::con_d_coord(:,:),grobal_grid(:,:),grobal_grid_es(:,:),nod_coord(:,:),zerovec(:),uvec(:)
  
  	integer grobal_grid_max,m,s,m_nod,s_nod,con_max,step,&	
  	sla_nod_max,i,j,k,l,o,p,q,nei_nod,nei_nod_1,nei_nod_2,&	
  	nn,nts_elem_max,x2,x11,x12,surn1,surn2
  
  	real(real64) gn,gn_tr,tol,tol_rm,ll,lx,ly,x,y,z,norm_rvec,norm_uvec,start,fin_time,nts_time,gzi
  
	! only for 2D, 2domains
	if(.not. allocated(obj%sur_nod_inf) )then
		allocate(obj%sur_nod_inf(2,2) )

		call obj%femdomain1%mesh%getSurface()
		call obj%femdomain2%mesh%getSurface()
		surn1 = size(obj%femdomain1%mesh%SurfaceLine2D)
		surn2 = size(obj%femdomain2%mesh%SurfaceLine2D)

		obj%sur_nod_inf(1,1) = 1
		obj%sur_nod_inf(1,2) = surn1
		obj%sur_nod_inf(2,1) = surn1+1
		obj%sur_nod_inf(2,2) = surn1+surn2

		if(.not. allocated(obj%surface_nod) )then
			allocate(obj%surface_nod(surn1+surn2))
			obj%surface_nod(1:surn1) = obj%femdomain1%mesh%SurfaceLine2D(:)
			obj%surface_nod(1+surn1:surn2) = obj%femdomain2%mesh%SurfaceLine2D(:)&
				+size(obj%femdomain1%mesh%nodcoord,1)
		endif
	endif

	con_max = 2
	step = obj%step
	uvec = obj%uvec

	p = size(obj%femdomain1%mesh%nodcoord,1)+size(obj%femdomain2%mesh%nodcoord,1)
	q = size(obj%femdomain1%mesh%nodcoord,2)


	if(.not.allocated(obj%nod_coord) ) allocate(obj%nod_coord(p,q))

	obj%nod_coord(1:size(obj%femdomain1%mesh%nodcoord,1),:) = obj%femdomain1%mesh%nodcoord(:,:)
	obj%nod_coord(size(obj%femdomain1%mesh%nodcoord,1)+1:&
		size(obj%femdomain2%mesh%nodcoord,1),:) = obj%femdomain2%mesh%nodcoord(:,:)


	if(.not.allocated(obj%elem_nod)) allocate(obj%elem_nod(p,q))

	obj%elem_nod(1:size(obj%femdomain1%mesh%elemnod,1),:) = obj%femdomain1%mesh%elemnod(:,:)
	obj%elem_nod(size(obj%femdomain1%mesh%elemnod,1)+1:&
		size(obj%femdomain2%mesh%elemnod,1),:) = obj%femdomain2%mesh%elemnod(:,:)&
		+size(obj%femdomain1%mesh%nodcoord,1)


	
  	allocate(nod_coord(size(obj%nod_coord,1),size(obj%nod_coord,2)),&
		zerovec(size(uvec)))

  	do i=1, size(nod_coord,1)
		nod_coord(i,1)=obj%nod_coord(i,1)+obj%uvec(2*i-1)
		nod_coord(i,2)=obj%nod_coord(i,2)+obj%uvec(2*i  )
	  	enddo
	  	zerovec(:)=0.0d0

	!===============================
	!contact search
	!=========================================================================================
	!Grobal search
	!----------------
	  
	allocate(con_d_coord(con_max,4))
	! 連続体ごと外接する長方形のx-min,x-max,y-min,y-maxの座標
	con_d_coord(1:con_max,1:4) = 0
	    do i = 1, con_max   ! 連続体ループ
		
		    do j = obj%sur_nod_inf(i,1), obj%sur_nod_inf(i,2) !該当連続体の開始節点～最終節点
		    !各連続体ごとに、最初の節点の値を初期のx-min,x-max,y-min,y-maxの座標とする。
	  	        if(j == obj%sur_nod_inf(i,1)) then
			        con_d_coord(i,1) = nod_coord( obj%surface_nod(j),1)
				    con_d_coord(i,2) = nod_coord( obj%surface_nod(j),1)
				    con_d_coord(i,3) = nod_coord( obj%surface_nod(j),2)
				    con_d_coord(i,4) = nod_coord( obj%surface_nod(j),2)
			    endif      
			  
			  
		        !連続体ごとに、接点の読み込み、最小/最大の更新
		        if(con_d_coord(i,1) > nod_coord( obj%surface_nod(j) ,1)) then
			        con_d_coord(i,1) = nod_coord( obj%surface_nod(j) ,1)
			    endif
			
			    if(con_d_coord(i,2) < nod_coord( obj%surface_nod(j) ,1)) then
			     con_d_coord(i,2) = nod_coord( obj%surface_nod(j) ,1)
			    endif
			
			    if(con_d_coord(i,3) > nod_coord( obj%surface_nod(j) ,2)) then
			        con_d_coord(i,3) = nod_coord( obj%surface_nod(j) ,2)
			    endif
			
			    if(con_d_coord(i,4) < nod_coord( obj%surface_nod(j) ,2)) then
			        con_d_coord(i,4) = nod_coord( obj%surface_nod(j) ,2)
			    endif
			  
			  
		    enddo
	    enddo

	! この時点で、連続体ごとに外接長方形の領域が確定
	!check

	grobal_grid_max = 0

	allocate(mast_slav(1,2))
	  
	  
	  ! grobal search のループ
    do i = 1, con_max
     
	     do j = 1, con_max
	    
		    if (i >= j) then
			   cycle
		    endif
		      ! 矩形接触判定
			  
			   if (con_d_coord(i,2) < con_d_coord(j,1)) then
			      cycle
			   elseif (con_d_coord(j,2) < con_d_coord(i,1)) then
				  cycle
			   elseif (con_d_coord(i,4) < con_d_coord(j,3)) then
				   cycle
			   elseif (con_d_coord(j,4) < con_d_coord(i,3)) then
                    cycle
               else	
                   !接触あり
		        	!退避用mast_slav_esの作成							
                 
					
					if(grobal_grid_max==0)then
						mast_slav(1,1)= i
						mast_slav(1,2)= j
	
						grobal_grid_max = grobal_grid_max + 1
					else
						allocate(mast_slav_es((size(mast_slav,1)),2))
						do k = 1, grobal_grid_max
							do l = 1, 2
								mast_slav_es(k,l) = mast_slav(k,l)
							enddo
						enddo
						
						deallocate(mast_slav)
						allocate(mast_slav(grobal_grid_max+1,2))
						!データの再格納
						do k = 1, grobal_grid_max			
							do l = 1, 2		
								mast_slav(k,l) = mast_slav_es(k,l)	   
							enddo		   
						enddo			
						mast_slav(grobal_grid_max+1,1)= i
						mast_slav(grobal_grid_max+1,2)= j
							
						grobal_grid_max = grobal_grid_max + 1
						deallocate(mast_slav_es)
					endif
				endif
	        enddo
    enddo
 
	if(grobal_grid_max/=0)then
 
 
		!grobal_grid_maxのリセット
		grobal_grid_max=size(mast_slav,1)
		!接触ありのmaster-slaveに対して、接触領域の確定・保存
		allocate(grobal_grid(size(mast_slav,1),4))
		do i = 1, grobal_grid_max
			do j = 1,4  
				grobal_grid(i,j) = 0.0d0
			enddo
		enddo
		!以下、xに関して確定・保存
		do k = 1, grobal_grid_max
			i=mast_slav(k,1) 
			j=mast_slav(k,2) 
	   
			if(con_d_coord(i,1)+con_d_coord(i,2) <= &
				con_d_coord(j,1)+con_d_coord(j,2)) then
				
				! iのx方向辺の中心<=jのx方向辺の中心
				if(con_d_coord(i,2) >= con_d_coord(j,2)) then
					!(3)に決定
					grobal_grid(k,1) = con_d_coord(j,1) ! x-min			   
					grobal_grid(k,2) = con_d_coord(j,2) ! x-max
			   
				else
					if(con_d_coord(i,1) >= con_d_coord(j,1)) then
					!(2)に決定
					grobal_grid(k,1) = con_d_coord(i,1) ! x-min	
					grobal_grid(k,2) = con_d_coord(i,2) ! x-max
				  
					else
					!(1)に決定
					grobal_grid(k,1) = con_d_coord(j,1) ! x-min	
					grobal_grid(k,2) = con_d_coord(i,2) ! x-max
					endif
				endif
			else			 
				! iのx方向辺の中心>jのx方向辺の中心
				if(con_d_coord(j,2) >= con_d_coord(i,2)) then
					!(3)に決定
					grobal_grid(k,1) = con_d_coord(i,1) ! x-min			   
					grobal_grid(k,2) = con_d_coord(i,2) ! x-max			   	   
				else
					if(con_d_coord(j,1) >= con_d_coord(i,1)) then
					!(2)に決定
					grobal_grid(k,1) = con_d_coord(j,1) ! x-min	
					grobal_grid(k,2) = con_d_coord(j,2) ! x-max	    
					else
					!(1)に決定
					grobal_grid(k,1) = con_d_coord(i,1) ! x-min	
					grobal_grid(k,2) = con_d_coord(j,2) ! x-max			  
					endif
				endif		  	  
			endif
		enddo

		!以下、yに関して確定・保存
		do k = 1, grobal_grid_max ! 接触組み合わせごとにループ
			i=mast_slav(k,1)
			j=mast_slav(k,2)  
	   
			if ((con_d_coord(i,3)+con_d_coord(i,4))/2 <= &
				(con_d_coord(j,3)+con_d_coord(j,4))/2)then
				! iのx方向辺の中心<=jのx方向辺の中心
				if(con_d_coord(i,4) >= con_d_coord(j,4)) then
				!(3)に決定
				grobal_grid(k,3) = con_d_coord(j,3) ! y-min			   
				grobal_grid(k,4) = con_d_coord(j,4) ! y-max
				else
					if(con_d_coord(i,3) >= con_d_coord(j,3))then
					!(2)に決定
					grobal_grid(k,3) = con_d_coord(i,3) ! y-min	
					grobal_grid(k,4) = con_d_coord(i,4) ! y-max
					else
					!(1)に決定
					grobal_grid(k,3) = con_d_coord(j,3) ! y-min	
					grobal_grid(k,4) = con_d_coord(i,4) ! y-max
					endif
				endif
			else			 
				! iのx方向辺の中心>jのx方向辺の中心
				if(con_d_coord(j,4) >= con_d_coord(i,4))then
					!(3)に決定
					grobal_grid(k,3) = con_d_coord(i,3) ! y-min			   
					grobal_grid(k,4) = con_d_coord(i,4) ! y-max			    
				else
					if(con_d_coord(j,3) >= con_d_coord(i,3))then
						!(2)に決定
						grobal_grid(k,3) = con_d_coord(j,3) ! y-min	
						grobal_grid(k,4) = con_d_coord(j,4) ! y-max   
					else
						!(1)に決定
						grobal_grid(k,3) = con_d_coord(i,3) ! x-min	
						grobal_grid(k,4) = con_d_coord(j,4) ! x-max			  
					endif
				endif		  
			endif
		enddo
		! この時点で、連続体組み合わせごとの重複領域（grobal search grid）が確定
		write(*,*) "Grobal search was succeed!"	 
	
	
	
		write(20,*) 'grobal grid, xmin xmax ymin ymax'
		do k = 1,size(grobal_grid,1) 
			write(20,*)grobal_grid(k,1),&
			grobal_grid(k,2),&
			grobal_grid(k,3),&
			grobal_grid(k,4)		 
		enddo

		!====================================================================================
		! Local search
		!----------------------	  

		do i=1, grobal_grid_max !重複矩形ごとループ
			m = 0  !master,slave各nodの数を記録する変数のリセット
			s = 0
			allocate(master_nod(1)) !master-slaveごとに接点番号記録用配列の用意
			allocate(slave_nod(1))
			master_nod(:)=0
			slave_nod(:)=0
		
			do k = 1, 2 !master矩形,slave矩形
				write(20,*) 'master,slave',k
		   
				do j = obj%sur_nod_inf(mast_slav(i,k),1), obj%sur_nod_inf &
					(mast_slav(i,k),2) !m,sごとに、表面節点を1つずつ、重複矩形に入っているか吟味 jは吟味中の表面接点用No.
				
					if(grobal_grid(i,1) <= nod_coord(obj%surface_nod(j),1) .and. & 
						nod_coord(obj%surface_nod(j),1) <= grobal_grid(i,2) ) then
					
						if(grobal_grid(i,3) <= nod_coord(obj%surface_nod(j),2) &
							.and. nod_coord(obj%surface_nod(j),2) <= grobal_grid(i,4)) then

							if (k == 1) then  
								m = m + 1    !master,slaveごとに接点数記録
								!接点数の記録		
								
								if (m == 1) then

									master_nod(m) = obj%surface_nod(j)
									
								elseif(m>=2) then
									!m>=2である。
									!master_nod配列の拡張
									allocate(master_nod_es(m-1))
									

									
									do l = 1,size(master_nod)  !_esへの接点番号の避難
										master_nod_es(l) = master_nod(l)
									enddo

									deallocate(master_nod)
									allocate(master_nod(m))   !_esから接点番号の再格納
									do l = 1,size(master_nod_es)
										master_nod(l) = master_nod_es(l)
									enddo 
									master_nod(m) = obj%surface_nod(j)
									deallocate(master_nod_es) !避難用配列の解体
				

								else
									 stop "ERROR Local Search m<1"
								endif

								
							 elseif(k ==2) then
								s = s + 1 !master,slaveごとに接点数記録
									!接点数の記録						  
								if (s == 1) then

									slave_nod(s) = obj%surface_nod(j)
									write(20,*)obj%surface_nod(j)
								elseif(s>=2)then
									!s>=2である。
									!slave_nod配列の拡張
									
									allocate(slave_nod_es(s-1))
						  
									do l = 1,size(slave_nod)  !_esへの接点番号の避難
										slave_nod_es(l) = slave_nod(l)
									enddo
					
									deallocate(slave_nod)
									allocate(slave_nod(s))   !_esから接点番号の再格納
									do l = 1,size(slave_nod_es)
										slave_nod(l) = slave_nod_es(l)
									enddo
									slave_nod(s) = obj%surface_nod(j)
									deallocate(slave_nod_es) !避難用配列の解体
									

								else
									 stop "ERROR Local Search m<2"
								endif

							else
								 stop 'L388 masterでもslaveでもないk/=1,2'
							endif

						else
							cycle !次節点へ
						endif 
					else
						cycle ! 次節点へ
					endif
				enddo
				 
			enddo

			!-------重複矩形の表面節点の出力					  
			write(20,*)'grobal_grid No.=',i
			write(20,*)'master_nod'

			do l=1,size(master_nod)
				write(20,*) master_nod(l)
			enddo
		
			write(20,*)'slave_nod',size(slave_nod)			
			do l=1,size(slave_nod)
				write(20,*) slave_nod(l)
			enddo			
			!----------------------

			!重複矩形内接点数を計上終了
			if( slave_nod( size(slave_nod,1) )==0 .or. master_nod(  size(master_nod,1) ) == 0) then !重複矩形内に接点なし

				if(grobal_grid_max==i)then
					print *, "No contact !"
					allocate(obj%nts_elem_nod(1,3) ) !no contact >> nts_elem_nod==0
					obj%nts_elem_nod(:,:)=0
					exit
				else
					cycle !次重複矩形へ
				endif
			endif
			

			!================================================================ 
			! 以下、NTS-elementの生成
			!------------------------------------
			! (1) nts_element節点番号記憶配列の確保

			if (i >= 2) then  !NTSへの書き込みが2回目以上で、NTS節点番号記憶用配列の拡張を要する場合
				allocate(nts_elem_nod_es(size(obj%nts_elem_nod,1) ,3))
				nts_elem_nod_es(:,:)=0
				nts_elem_max=size(obj%nts_elem_nod,1) 
				do l = 1, size(obj%nts_elem_nod,1)
					do k =1, 3
						nts_elem_nod_es(l,k) = obj%nts_elem_nod(l,k)
					enddo
				enddo

				deallocate(obj%nts_elem_nod)
				allocate(obj%nts_elem_nod(size(nts_elem_nod_es,1)+size(slave_nod,1),3))
				obj%nts_elem_nod(:,:)=0
				! size=これまでに記録されたntsの数+今回のslave_nodの数
				do l = 1, size(nts_elem_nod_es,1)
					do k =1, 3
						obj%nts_elem_nod(l,k) = nts_elem_nod_es(l,k)
					enddo
				enddo
				deallocate(nts_elem_nod_es)
			elseif(i==1)then
				nts_elem_max =0
				allocate(obj%nts_elem_nod( size(slave_nod) ,3))
				obj%nts_elem_nod(:,:)=0
			else
				 stop  "wrong i on module ntselem"
			endif

			!nts_elem_nodを拡張済み
			!-------------------------------------------------------------------------------------------
			!initial value
			nei_nod=0
			nei_nod_1=0
			do l = 1, size(slave_nod) !slave nod ごとにNTS作成

				!do k = 1,size(master_nod)!重複矩形を構成するmaster_nodを1つずつ検証
				do k = obj%sur_nod_inf(mast_slav(i,1),1), obj%sur_nod_inf(mast_slav(i,1),2)
					!表面節点用No.
					!現在のslave_nodとの距離を計算

					lx=(nod_coord(slave_nod(l),1)-nod_coord(obj%surface_nod(k),1))**2
					ly=(nod_coord(slave_nod(l),2)-nod_coord(obj%surface_nod(k),2))**2

					gn_tr = (lx+ly)**(1.0d0/2.0d0)

					if(k==1) then !初期値
						gn=gn_tr
					endif
				  
					!汝は最近傍なりや?
					If(gn_tr <= gn) then
						gn = gn_tr 
						nei_nod=obj%surface_nod(k) !近傍節点番号の更新X1@表面節点用No.
					elseif(gn_tr >gn) then
						cycle
					else
						 stop  'something is wrong at detecting x_11'
					endif

				enddo

				!最近傍節点=nei_nod---------------------

				
				obj%nts_elem_nod(nts_elem_max+l,1) = slave_nod(l)
				obj%nts_elem_nod(nts_elem_max+l,2) = nei_nod

			enddo
			!次重複矩形へ、パラメータクリア
			m = 0
			s = 0
			deallocate(master_nod)
			deallocate(slave_nod)
		
		enddo
	elseif(grobal_grid_max==0)then
		print *, "No contact !"
		allocate(obj%nts_elem_nod(1,3) ) !no contact >> nts_elem_nod==0
		obj%nts_elem_nod(:,:)=0
	else
		 stop "Wrong value in grobal_grid"
	endif
	
	deallocate(nod_coord,zerovec)



 end subroutine 

!=====================================================================
 subroutine ls_nts_materialCM(obj)
	class(ContactMechanics_),intent(inout) :: obj
	integer i,j,s,m,ss,mm,n,step

	step = obj%step
	
	n=size(obj%nts_elem_nod,1)

	if(allocated(obj%nts_mat) )then
		deallocate(obj%nts_mat)
	endif

	allocate(obj%nts_mat(n) )

	do i=1,n !nts要素ごとに繰り返し
		if(obj%nts_elem_nod(1,1)+obj%nts_elem_nod(1,2)+obj%nts_elem_nod(1,3)==0 )then
			obj%nts_mat(:)=0 !0を入れておく

			exit
		endif
!		if(step==136) stop "2"
		!表面節点No.を検索し、slave=s,master=mへ格納
		do j=1, size(obj%surface_nod,1)
			if(obj%surface_nod(j)==obj%nts_elem_nod(i,1) )then
				s=j

			elseif(obj%surface_nod(j)==obj%nts_elem_nod(i,2) )then
				m=j

			else
				cycle
			endif
		enddo


		!master nodの周面材料No.を検索
		
		do j=1,size(obj%sur_inf_mat,1)
			if(obj%sur_inf_mat(j,1)<=m .and. obj%sur_inf_mat(j,2)>=m )then
				mm=obj%sur_inf_mat(j,3)

				exit
			else
				cycle
			endif
		enddo
		

		!slave  nodの周面材料No.を検索
		do j=1,size(obj%sur_inf_mat,1)
			if(obj%sur_inf_mat(j,1)<=s .and. obj%sur_inf_mat(j,2)>=s )then
				ss=obj%sur_inf_mat(j,3)

				exit
			else
				cycle
			endif
		enddo
		obj%nts_mat(i)=obj%contact_mat(ss,mm)	
	enddo
	
 end subroutine 
!=====================================================================
 subroutine save_nts_element(nts_elem_nod,nts_amo,old_nts_elem_nod,old_nts_amo,surface_nod,sur_nod_inf,&
	stick_slip,old_stick_slip)
	real(real64),intent(in)::nts_amo(:,:)
	real(real64),allocatable,intent(inout)::old_nts_amo(:,:)
	real(real64) gzin
	integer,intent(in)::nts_elem_nod(:,:),surface_nod(:),sur_nod_inf(:,:),stick_slip(:)
	integer,allocatable,intent(inout)::old_nts_elem_nod(:,:),old_stick_slip(:)
	integer i,j,n,m1,m2,m3,shift,slave_node,old_master,master1,master2
	
	n=size(nts_elem_nod,1)
	m1=size(nts_elem_nod,2)
	m2=size(nts_amo,2)
	if( allocated(old_nts_amo) )deallocate(old_nts_amo)
	if( allocated(old_nts_elem_nod) )deallocate(old_nts_elem_nod)
	if( allocated(old_stick_slip) )deallocate(old_stick_slip)
	
	
	allocate( old_nts_elem_nod(n,m1),old_nts_amo(n,m2),old_stick_slip(n)  )
	
	old_nts_elem_nod(:,:)=nts_elem_nod(:,:)
	old_nts_amo(:,:)=nts_amo(:,:)
	old_stick_slip(:)=stick_slip(:)
	
	do i=1,n
		gzin=nts_amo(i,10) !converged gzi
		if(gzin>1.0d0)then
			shift=1
			slave_node=nts_elem_nod(i,1)
			old_master=nts_elem_nod(i,2)
			old_master=nts_elem_nod(i,3)
			call get_next_segment(surface_nod,sur_nod_inf,shift,old_master,master1,master2)
			
			old_nts_elem_nod(i,2)=master1
			old_nts_elem_nod(i,3)=master2
			
			gzin=0.0d0
			
		elseif(gzin<0.0d0)then
			shift=-1
			slave_node=nts_elem_nod(i,1)
			old_master=nts_elem_nod(i,2)
			old_master=nts_elem_nod(i,3)
			call get_next_segment(surface_nod,sur_nod_inf,shift,old_master,master1,master2)
			
			old_nts_elem_nod(i,2)=master1
			old_nts_elem_nod(i,3)=master2
			gzin=1.0d0
		else
			cycle
		endif
		old_nts_amo(i,:)=0.0d0
		old_nts_amo(i,1)=gzin
		old_nts_amo(i,12)=nts_amo(i,12)
	enddo
	
 end subroutine save_nts_element
!=====================================================================
 subroutine get_next_segment(surface_nod,sur_nod_inf,shift,old_master,master1,master2)
	integer,intent(in)::surface_nod(:),sur_nod_inf(:,:),shift,old_master
	integer,intent(out)::master1,master2
	integer i,surface_nod_ID,domain_number,first_ID,last_ID
	
	if(shift==1)then
		!case of old_master2
		surface_nod_ID=0
		do i=1,size(surface_nod)
			if(surface_nod(i)==old_master )then
				surface_nod_ID=i
				exit
			else
				cycle
			endif
		enddo
		
		domain_number=0
		do i=1,size(sur_nod_inf)
			first_ID=sur_nod_inf(i,1)
			last_ID =sur_nod_inf(i,2)
			if( first_ID<=surface_nod_ID .and. surface_nod_ID<=last_ID)then
				domain_number=i
				exit
			else
				cycle
			endif
		enddo
		
		if(domain_number==0 .or. surface_nod_ID==0)then
			 stop "invalid slave node ID: sub. get_next_segment"
		endif
		
		first_ID=sur_nod_inf(domain_number,1)
		last_ID =sur_nod_inf(domain_number,2)
		
		if(surface_nod_ID==last_ID)then
			master1=surface_nod(last_ID)
			master2=surface_nod(first_ID)
		else
			master1=surface_nod(surface_nod_ID)
			master2=surface_nod(surface_nod_ID+1)
		endif
		
	elseif(shift==-1)then
		surface_nod_ID=0
		do i=1,size(surface_nod)
			if(surface_nod(i)==old_master )then
				surface_nod_ID=i
				exit
			else
				cycle
			endif
		enddo
		
		domain_number=0
		do i=1,size(sur_nod_inf)
			first_ID=sur_nod_inf(i,1)
			last_ID =sur_nod_inf(i,2)
			if( first_ID<=surface_nod_ID .and. surface_nod_ID<=last_ID)then
				domain_number=i
				exit
			else
				cycle
			endif
		enddo
		
		if(domain_number==0 .or. surface_nod_ID==0)then
			 stop "invalid slave node ID: sub. get_next_segment"
		endif
		
		first_ID=sur_nod_inf(domain_number,1)
		last_ID =sur_nod_inf(domain_number,2)
		
		if(surface_nod_ID==first_ID)then
			master1=surface_nod(last_ID)
			master2=surface_nod(first_ID)
		else
			master1=surface_nod(surface_nod_ID-1)
			master2=surface_nod(surface_nod_ID)
		endif	
	else
		 stop "invalid shifting parameter : sub.get_next_segment"
	endif
	
	
 end subroutine get_next_segment
!=====================================================================
 subroutine load_nts_element(nts_elem_nod,nts_amo,old_nts_elem_nod,old_nts_amo,stick_slip,old_stick_slip)
	real(real64),intent(inout)::nts_amo(:,:)
	real(real64),intent(in)::old_nts_amo(:,:)
	integer,intent(inout)::nts_elem_nod(:,:),stick_slip(:)
	integer,intent(in)::old_nts_elem_nod(:,:),old_stick_slip(:)
	
	integer i,j,n
	
	do i=1,size(nts_elem_nod,1)
		n=0
		do j=1,size(old_nts_elem_nod,1)
			if(old_nts_elem_nod(j,1)==nts_elem_nod(i,1) )then
				n=j
				exit
			else
				cycle
			endif
		enddo
		
		if(n==0)then
			cycle
		else
			!nts_elem_nod(i,2)=old_nts_elem_nod(n,2)
			!nts_elem_nod(i,3)=old_nts_elem_nod(n,3)
			nts_amo(i,:)=old_nts_amo(i,:)
			!stick_slip(i)=old_stick_slip(n)
		endif
		
	enddo
 end subroutine load_nts_element
!=====================================================================
 subroutine ls_get_stabilized_ntsCM(obj)
	class(ContactMechanics_),intent(inout) :: obj
	integer,allocatable::nts_elem_nod_new(:,:)
	integer i,j,k,n,node_num,old_master,master1,master2,shift,cs,cm
	


	if(obj%nts_elem_nod(1,1)+obj%nts_elem_nod(1,2)+obj%nts_elem_nod(1,3)==0 )then
		return
	endif
	!expand nts_lem_nod from 3 to 6
	n=size(obj%nts_elem_nod,1)
	allocate(nts_elem_nod_new(n,6) )
	
	!input node#1 and node #2
	do i=1,n
		nts_elem_nod_new(i,1:2)=obj%nts_elem_nod(i,1:2)
		
		!get node#3
		old_master=nts_elem_nod_new(i,2)
		shift=1
		call get_next_segment(obj%surface_nod,obj%sur_nod_inf,shift,old_master,master1,master2)
		nts_elem_nod_new(i,3)=master2
		
		!get node#4
		old_master=nts_elem_nod_new(i,2)
		shift=-1
		call get_next_segment(obj%surface_nod,obj%sur_nod_inf,shift,old_master,master1,master2)
		nts_elem_nod_new(i,4)=master1
		
		!get node#5
		old_master=nts_elem_nod_new(i,3)
		shift=1
		call get_next_segment(obj%surface_nod,obj%sur_nod_inf,shift,old_master,master1,master2)
		nts_elem_nod_new(i,5)=master2
		
		!get node#6
		old_master=nts_elem_nod_new(i,4)
		shift=-1
		call get_next_segment(obj%surface_nod,obj%sur_nod_inf,shift,old_master,master1,master2)
		nts_elem_nod_new(i,6)=master1
	enddo
	
	deallocate(obj%nts_elem_nod)
	allocate(obj%nts_elem_nod(n,6))
	do i=1,n
		obj%nts_elem_nod(i,1:6)=nts_elem_nod_new(i,1:6)
	enddo
	
 end subroutine 
!=====================================================================


! #########################################
subroutine setPenaltyParaCM(obj,para)
	class(ContactMechanics_),intent(inout)::obj
	real(real64),intent(in)		::	para

	obj%PenaltyPara = para

end subroutine
! #########################################
  

! #########################################
subroutine updateContactStressCM(obj)
	class(ContactMechanics_),intent(inout)::obj
	!type(MPI_)::mpidata
	
	

	if(.not. allocated(obj%FEMIface%NTS_ElemNod) )then
		call obj%FEMIface%GetFEMIface()
	endif
	! check NTS
	!call showArray(obj%FEMIface%Mesh1%NodCoord,IndexArray=obj%FEMIface%NTS_ElemNod(:,1:1)&
	!	,Name="checkNTSmesh1.txt" )
	!call showArray(obj%FEMIface%Mesh2%NodCoord,IndexArray=obj%FEMIface%NTS_ElemNod(:,2: )&
	!	,Name="checkNTSmesh2.txt" )
	!call showArray(obj%FEMIface%FEMDomains(1)%FEMDomainp%Mesh%NodCoord,&
	!	IndexArray=obj%FEMIface%NTS_ElemNod(:,1:1),Name="checkNTSdomain2.txt" )
	!call showArray(obj%FEMIface%FEMDomains(2)%FEMDomainp%Mesh%NodCoord,&
	!	IndexArray=obj%FEMIface%NTS_ElemNod(:,2: ),Name="checkNTSdomain2.txt" )
	!call showArray(obj%FEMIface%Mesh1%NodCoord, Name="checkNTSmesh1.txt" )
	!call showArray(obj%FEMIface%Mesh2%NodCoord, Name="checkNTSmesh2.txt" )
	!call showArray(obj%FEMIface%Mesh2%NodCoord, Name="checkNTSmesh2.txt" )
	!call showArray(obj%FEMIface%Mesh1%ElemNod,Name="checkNTSmesh1.txt" )
	!call showArray(obj%FEMIface%Mesh2%ElemNod,Name="checkNTSmesh2.txt" )
	!call showArray(obj%FEMIface%NTS_ElemNod,Name="checkNTSmesh3.txt" ) !wrong pointer
	!call showArray(obj%FEMIface%NTS_ElemNod,Name="checkNTSmesh4.txt" ) !wrong pointer
	
	
	
	call obj%getGap()
	
	call obj%getForce()
	
	call obj%exportForceAsTraction()



end subroutine
! #########################################


! #########################################
subroutine getGapCM(obj)
	class(ContactMechanics_),intent(inout)::obj
	real(real64),allocatable :: gap(:),avec(:),avec1(:),avec2(:),nvec(:),evec(:),xs1(:),xm1(:),xm2(:),xm3(:),xm4(:)
	real(real64),allocatable :: xm5(:),xm6(:),xm7(:),xm8(:),mid(:)
	real(real64) :: val
	integer :: i,j,k,n,NumOfNTSelem,dim_num
	!type(MPI_)::mpidata

	if(.not. allocated(obj%FEMIface%NTS_ElemNod) )then
		print *, "Error :: ContactMechanics_ >> updateContactStressCM >> not (.not. allocated(obj%NTS_ElemNod) )"
		return	
	endif

	

	NumOfNTSelem=size(obj%FEMIface%NTS_ElemNod,1)

	
	dim_num=size(obj%FEMIface%FEMDomains(1)%FEMDomainp%Mesh%NodCoord,2 ) 
	
	

	allocate(gap(dim_num) )
	allocate(avec(3) )
	allocate(avec1(3) )
	allocate(avec2(3) )
	allocate(nvec(3) )
	allocate(evec(3) )
	allocate(xs1(3))
	allocate(xm1(3))
	allocate(xm2(3))
	allocate(xm3(3))
	allocate(xm4(3))
	allocate(xm5(3))
	allocate(xm6(3))
	allocate(xm7(3))
	allocate(xm8(3))
	allocate(mid(3))

	! initial :: inactive
	gap=0.0d0
	avec(:)=0.0d0
	avec1(:)=0.0d0
	avec2(:)=0.0d0
	nvec(:)=0.0d0
	evec(:)=0.0d0
	evec(3)=1.0d0
	xs1(:)=0.0d0
	xm1(:)=0.0d0
	xm2(:)=0.0d0
	xm3(:)=0.0d0
	xm4(:)=0.0d0
	xm5(:)=0.0d0
	xm6(:)=0.0d0
	xm7(:)=0.0d0
	xm8(:)=0.0d0
	mid(:)=0.0d0


	

	if(.not.allocated(obj%NTSGap))then
		allocate( obj%NTSGap(NumOfNTSElem,dim_num) )
		obj%NTSGap(:,:)=0.0d0
	elseif( size(obj%NTSGap,1)/=NumOfNTSElem )then
		deallocate(obj%NTSGap)
		allocate( obj%NTSGap(NumOfNTSElem,dim_num) )
		obj%NTSGap(:,:)=0.0d0
	else
		obj%NTSGap(:,:)=0.0d0
	endif


	if(.not.allocated(obj%NTSGzi))then
		allocate( obj%NTSGzi(NumOfNTSElem,dim_num) )
		obj%NTSGzi(:,:)=0.0d0
	elseif( size(obj%NTSGzi,1)/=NumOfNTSElem )then
		deallocate(obj%NTSGzi)
		allocate( obj%NTSGzi(NumOfNTSElem,dim_num) )
		obj%NTSGzi(:,:)=0.0d0
	else
		obj%NTSGzi(:,:)=0.0d0
	endif
	

	if(dim_num==2)then
		! 2-D NTS
		do i=1,NumOfNTSElem
			print *, "CmClass getGap not validated"
			xs1(	1:2)=obj%FEMIface%NTS_NodCoord(i,1:2)
			xm1(1:2)=obj%FEMIface%NTS_NodCoord(i,3:4)
			xm2(1:2)=obj%FEMIface%NTS_NodCoord(i,5:6)
			avec(1:2)=xm2(1:2)-xm1(1:2)
			nvec(1:3)=cross_product(evec,avec)
			val=norm(nvec)
			if(val==0.0d0)then
				print *, "norm = ",val
				stop "ERROR CMClass >> getGap"
			endif
			nvec(:)=1.0d0/val*nvec(:)
			obj%NTSGap(i,1:2)=dot_product( xs1(1:2)-xm1(1:2),nvec(1:2)  )
			print *, "gap=",dot_product(obj%NTSGap(i,1:2),nvec(1:2) )
		enddo
	elseif(dim_num==3)then
		! 3-D NTS
	
		do i=1,NumOfNTSElem
			
			xs1(1:3)=obj%FEMIface%NTS_NodCoord(i, 1:	3)
			xm1(1:3)=obj%FEMIface%NTS_NodCoord(i, 4:	6)
			xm2(1:3)=obj%FEMIface%NTS_NodCoord(i, 7:	9)
			xm3(1:3)=obj%FEMIface%NTS_NodCoord(i,10:	12)
			xm4(1:3)=obj%FEMIface%NTS_NodCoord(i,13:	15)
			
			mid(:)=0.250d0*xm1(:)+0.250d0*xm2(:)+0.250d0*xm3(:)+0.250d0*xm4(:)
			avec1(1:3)=xm1(1:3)-mid(1:3)
			avec2(1:3)=xm2(1:3)-mid(1:3)
			nvec(1:3)=cross_product(avec1,avec2)
			val=norm(nvec)
			if(val==0.0d0)then
				print *, "norm = ",val
				stop "ERROR CMClass >> getGap"
			endif
			nvec(:)=1.0d0/val*nvec(:)
			obj%NTSGap(i,1:3)=dot_product( xs1(1:3)-mid(1:3),nvec(1:3)  )
			

			!print *, dot_product(obj%NTSGap(i,1:3),nvec)
			!write(1010,*) " " 
			!write(1010,*) xs1(1:3)
			!write(1010,*) mid(1:3)
			!write(1010,*) " " 
			!write(1010,*) xm1(1:3)
			!write(1010,*) xm2(1:3)
			!write(1010,*) xm3(1:3)
			!write(1010,*) xm4(1:3)
			!write(1010,*) xm1(1:3)
			!write(1020,*) mid(1:3),xs1(1:3)-mid(1:3) 
			!write(1030,*) mid(1:3),obj%NTSGap(i,1:3) 
			!print *, "gap=",dot_product(obj%NTSGap(i,1:3),nvec(1:3) )
		enddo
	else
		print *, "Dimension of coord = ",dim_num
		stop "getGapCM >> invalid dimension"
	endif
	

end subroutine
! #########################################


! #########################################
subroutine getForceCM(obj)
	class(ContactMechanics_),intent(inout)::obj
	real(real64),allocatable :: gap(:),avec(:),avec1(:),avec2(:),nvec(:),evec(:),xs1(:),xm1(:),xm2(:),xm3(:),xm4(:)
	real(real64),allocatable :: xm5(:),xm6(:),xm7(:),xm8(:),mid(:)
	real(real64) :: val,area
	integer :: i,j,k,n,m,NumOfNTSelem,dim_num

	real(real64) :: gzi,gzi1,gzi2

	if(.not. allocated(obj%FEMIface%NTS_ElemNod) )then
		print *, "Error :: ContactMechanics_ >> updateContactStressCM >> not (.not. allocated(obj%NTS_ElemNod) )"
		return	
	endif


	NumOfNTSelem=size(obj%FEMIface%NTS_ElemNod,1)

	dim_num=size(obj%FEMIface%FEMDomains(1)%FEMDomainp%Mesh%NodCoord,2 ) 

	

	allocate(gap(dim_num) )
	allocate(avec(3) )
	allocate(avec1(3) )
	allocate(avec2(3) )
	allocate(nvec(3) )
	allocate(evec(3) )
	allocate(xs1(3))
	allocate(xm1(3))
	allocate(xm2(3))
	allocate(xm3(3))
	allocate(xm4(3))
	allocate(xm5(3))
	allocate(xm6(3))
	allocate(xm7(3))
	allocate(xm8(3))
	allocate(mid(3))

	! initial :: inactive
	gap=0.0d0
	avec(:)=0.0d0
	avec1(:)=0.0d0
	avec2(:)=0.0d0
	nvec(:)=0.0d0
	evec(:)=0.0d0
	evec(3)=1.0d0
	xs1(:)=0.0d0
	xm1(:)=0.0d0
	xm2(:)=0.0d0
	xm3(:)=0.0d0
	xm4(:)=0.0d0
	xm5(:)=0.0d0
	xm6(:)=0.0d0
	xm7(:)=0.0d0
	xm8(:)=0.0d0
	mid(:)=0.0d0

	n=size(obj%FEMDomain1%Mesh%NodCoord,1)
	m=size(obj%FEMDomain2%Mesh%NodCoord,1)
	dim_num=size(obj%FEMDomain2%Mesh%NodCoord,2)
	if(.not.allocated(obj%Domain1Force) )then
		allocate(obj%Domain1Force(n,dim_num) )
		obj%Domain1Force(:,:)=0.0d0
	endif
	if(.not.allocated(obj%Domain2Force) )then
		allocate(obj%Domain2Force(m,dim_num) )
		obj%Domain2Force(:,:)=0.0d0
	endif

	gzi=0.0d0
	gzi1=0.0d0
	gzi2=0.0d0
	if(dim_num==2)then
		! import gzi at here
		! 2-D NTS
		do i=1,NumOfNTSElem
			print *, "CmClass getGap not validated"
			xs1(	1:2)=obj%FEMIface%NTS_NodCoord(i,1:2)
			xm1(1:2)=obj%FEMIface%NTS_NodCoord(i,3:4)
			xm2(1:2)=obj%FEMIface%NTS_NodCoord(i,5:6)
			avec(1:2)=xm2(1:2)-xm1(1:2)
			nvec(1:3)=cross_product(evec,avec)
			val=norm(nvec)
			if(val==0.0d0)then
				print *, "norm = ",val
				stop "ERROR CMClass >> getGap"
			endif
			nvec(:)=1.0d0/val*nvec(:)

		enddo
	elseif(dim_num==3)then
		! import gzi at here

		do i=1,NumOfNTSElem
			
			xs1(1:3)=obj%FEMIface%NTS_NodCoord(i, 1:	3)
			xm1(1:3)=obj%FEMIface%NTS_NodCoord(i, 4:	6)
			xm2(1:3)=obj%FEMIface%NTS_NodCoord(i, 7:	9)
			xm3(1:3)=obj%FEMIface%NTS_NodCoord(i,10:	12)
			xm4(1:3)=obj%FEMIface%NTS_NodCoord(i,13:	15)
			
			mid(:)=0.250d0*xm1(:)+0.250d0*xm2(:)+0.250d0*xm3(:)+0.250d0*xm4(:)
			avec1(1:3)=xm1(1:3)-mid(1:3)
			avec2(1:3)=xm2(1:3)-mid(1:3)
			nvec(1:3)=cross_product(avec1,avec2)
			val=norm(nvec)
			if(val==0.0d0)then
				print *, "norm = ",val
				stop "ERROR CMClass >> getGap"
			endif
			nvec(:)=1.0d0/val*nvec(:)
			obj%NTSGap(i,1:3)=dot_product( xs1(1:3)-mid(1:3),nvec(1:3)  )
			
			! Area
			area=1.0d0

			! compute ShapeFunc(:)
			! get contact force from penaltypara*gap*ShapeFunc(:)
			do j=1,size(obj%FEMIface%NTS_ElemNod,1)
				obj%Domain1Force(obj%FEMIface%NTS_ElemNod(j,1),1:3 )=&
				obj%Domain1Force(obj%FEMIface%NTS_ElemNod(j,1),1:3 )+&
				obj%penaltypara*obj%NTSGap(i,1:3)*area
			
				obj%Domain2Force(obj%FEMIface%NTS_ElemNod(j,2),1:3 )=&
				obj%Domain2Force(obj%FEMIface%NTS_ElemNod(j,2),1:3 )+&
				obj%penaltypara*obj%NTSGap(i,1:3)*area/4.0d0*(-1.0d0)
			
				obj%Domain2Force(obj%FEMIface%NTS_ElemNod(j,3),1:3 )=&
				obj%Domain2Force(obj%FEMIface%NTS_ElemNod(j,3),1:3 )+&
				obj%penaltypara*obj%NTSGap(i,1:3)*area/4.0d0*(-1.0d0)
			
				obj%Domain2Force(obj%FEMIface%NTS_ElemNod(j,4),1:3 )=&
				obj%Domain2Force(obj%FEMIface%NTS_ElemNod(j,4),1:3 )+&
				obj%penaltypara*obj%NTSGap(i,1:3)*area/4.0d0*(-1.0d0)
			
				obj%Domain2Force(obj%FEMIface%NTS_ElemNod(j,5),1:3 )=&
				obj%Domain2Force(obj%FEMIface%NTS_ElemNod(j,5),1:3 )+&
				obj%penaltypara*obj%NTSGap(i,1:3)*area/4.0d0*(-1.0d0)
			enddo


		enddo


	else
		print *, "Dimension of coord = ",dim_num
		stop "getForceCM >> invalid dimension"
	endif




end subroutine
! #########################################

! #########################################
subroutine exportForceAsTractionCM(obj)
	class(ContactMechanics_),intent(inout)::obj
	!type(mpi_)::mpidata
	integer :: nodeid,i,j,k
	real(real64) :: bcval

	
	
	do i=1,size(obj%FEMIface%NTS_ElemNod,1)
		
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! Really??
		print *, "slave node id : ",obj%FEMIface%NTS_ElemNod(i,1),"master node id : ",obj%FEMIface%NTS_ElemNod(i,2:)
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		do j=1,size(obj%FEMIface%NTS_ElemNod,2)

			do k=1,size(obj%Domain1Force,2)
				if(j==1)then
					
					!!!!!!!! debug !!!!!!!!!!!!!!!

					! which is the correct node_id?
					nodeid=obj%FEMIface%Mesh1%GlobalNodID(obj%FEMIface%NTS_ElemNod(i,j))
					!nodeid=obj%FEMIface%GloNodPoint1(obj%FEMIface%NTS_ElemNod(i,j))

					! obj%FEMIface%NTS_ElemNod is node pointer to local nodes, obj%FEMIface%Mesh%NodCoord
					!nodeid=obj%FEMIface%NTS_ElemNod(i,j)
					
					bcval = obj%Domain1Force(obj%FEMIface%NTS_ElemNod(i,j),k)
					!bcval = 0.0d0
					!bcval=zeroif(obj%NTSGap(i,k),positive=.true.)/10000.0d0
					if(k/=1)then
						bcval=0.0d0
					endif

					
					call obj%FEMDomain1%AddNBC(NodID=nodeid,DimID=k,Val=bcval,FastMode=.false.)
				else
					
					!!!!!!!! debug !!!!!!!!!!!!!!!
					nodeid=obj%FEMIface%Mesh2%GlobalNodID(obj%FEMIface%NTS_ElemNod(i,j))
					!nodeid=obj%FEMIface%GloNodPoint2(obj%FEMIface%NTS_ElemNod(i,j))
					!nodeid=obj%FEMIface%NTS_ElemNod(i,j)
					
					bcval = obj%Domain2Force(obj%FEMIface%NTS_ElemNod(i,j),k)

					!bcval = 0.0d0
					!bcval=zeroif(obj%NTSGap(i,k),positive=.true.)/10000.0d0

					if(k/=1)then
						bcval=0.0d0
					endif

					call obj%FEMDomain2%AddNBC(NodID=nodeid,DimID=k,Val=bcval,FastMode=.false.)
				endif
			enddo
		enddo
	enddo		
	
    !call showArray(obj%FEMDomain1%Mesh%NodCoord,IndexArray=obj%FEMIface%GloNodPoint1,Name="obj%GloNodPoint1.txt" )
    !call showArray(obj%FEMDomain2%Mesh%NodCoord,IndexArray=obj%FEMIface%GloNodPoint2,Name="obj%GloNodPoint2.txt" )
	
	call obj%FEMIface%GmshPlotMesh(Name="debugNTS",withNeumannBC=.true.,withDirichletBC=.true.)

	!!call mpidata%end()
	!stop "debug"	
end subroutine
! #########################################


! #########################################################
subroutine updateTimestepContact(obj,timestep)
    class(ContactMechanics_),intent(inout)::obj
    integer,optional,intent(in)::timestep
    
    call obj%FEMIFace%updateTimeStep(timestep=timestep)

end subroutine
! #########################################################


! #########################################################
subroutine getDispBoundCM(obj)
	class(ContactMechanics_),intent(inout) :: obj
	integer(int32) :: num_of_u_nod_x=0
	integer(int32) :: num_of_u_nod_y=0
	integer(int32) :: num_of_u_nod_z=0
	integer(int32) :: i,n,domain1_node_num

	! for x
	do i=1,size(obj%femdomain1%boundary%DBoundNodID,1)
		if(obj%femdomain1%boundary%DBoundNodID(i,1) >=1)then
			num_of_u_nod_x=num_of_u_nod_x+1
		endif
	enddo
	do i=1,size(obj%femdomain2%boundary%DBoundNodID,1)
		if(obj%femdomain2%boundary%DBoundNodID(i,1) >=1)then
			num_of_u_nod_x=num_of_u_nod_x+1
		endif
	enddo

	if(allocated(obj%u_nod_x) ) deallocate(obj%u_nod_x)
	if(allocated(obj%u_nod_dis_x) ) deallocate(obj%u_nod_dis_x)
	allocate(obj%u_nod_x(num_of_u_nod_x) )
	allocate(obj%u_nod_dis_x(num_of_u_nod_x) )

	num_of_u_nod_x=0
	do i=1,size(obj%femdomain1%boundary%DBoundNodID,1)
		if(obj%femdomain1%boundary%DBoundNodID(i,1) >=1)then
			num_of_u_nod_x=num_of_u_nod_x+1
			obj%u_nod_x(num_of_u_nod_x) = obj%femdomain1%boundary%DBoundNodID(i,1)
			obj%u_nod_dis_x(num_of_u_nod_x) = obj%femdomain1%boundary%DBoundVal(i,1)
		endif
	enddo

	do i=1,size(obj%femdomain2%boundary%DBoundNodID,1)
		if(obj%femdomain2%boundary%DBoundNodID(i,1) >=1)then
			num_of_u_nod_x=num_of_u_nod_x+1
			obj%u_nod_x(num_of_u_nod_x) = obj%femdomain2%boundary%DBoundNodID(i,1)
			obj%u_nod_dis_x(num_of_u_nod_x) = obj%femdomain2%boundary%DBoundVal(i,1)
		endif
	enddo

	if(size(obj%femdomain2%boundary%DBoundNodID,2)==1 )then
		return
	endif

	! for y
	do i=1,size(obj%femdomain1%boundary%DBoundNodID,1)
		if(obj%femdomain1%boundary%DBoundNodID(i,2) >=1)then
			num_of_u_nod_y=num_of_u_nod_y+1
		endif
	enddo
	do i=1,size(obj%femdomain2%boundary%DBoundNodID,1)
		if(obj%femdomain2%boundary%DBoundNodID(i,2) >=1)then
			num_of_u_nod_y=num_of_u_nod_y+1
		endif
	enddo

	if(allocated(obj%u_nod_y) ) deallocate(obj%u_nod_y)
	if(allocated(obj%u_nod_dis_y) ) deallocate(obj%u_nod_dis_y)
	allocate(obj%u_nod_y(num_of_u_nod_y) )
	allocate(obj%u_nod_dis_y(num_of_u_nod_y) )

	num_of_u_nod_y=0
	do i=1,size(obj%femdomain1%boundary%DBoundNodID,1)
		if(obj%femdomain1%boundary%DBoundNodID(i,2) >=1)then
			num_of_u_nod_y=num_of_u_nod_y+1
			obj%u_nod_y(num_of_u_nod_y) = obj%femdomain1%boundary%DBoundNodID(i,2)
			obj%u_nod_dis_y(num_of_u_nod_y) = obj%femdomain1%boundary%DBoundVal(i,2)
		endif
	enddo

	do i=1,size(obj%femdomain2%boundary%DBoundNodID,1)
		if(obj%femdomain2%boundary%DBoundNodID(i,2) >=1)then
			num_of_u_nod_y=num_of_u_nod_y+1
			obj%u_nod_y(num_of_u_nod_y) = obj%femdomain2%boundary%DBoundNodID(i,2)
			obj%u_nod_dis_y(num_of_u_nod_y) = obj%femdomain2%boundary%DBoundVal(i,2)
		endif
	enddo

	if(size(obj%femdomain2%boundary%DBoundNodID,2)==2 )then
		return
	endif

	! for z
	do i=1,size(obj%femdomain1%boundary%DBoundNodID,1)
		if(obj%femdomain1%boundary%DBoundNodID(i,3) >=1)then
			num_of_u_nod_z=num_of_u_nod_z+1
		endif
	enddo
	do i=1,size(obj%femdomain2%boundary%DBoundNodID,1)
		if(obj%femdomain2%boundary%DBoundNodID(i,3) >=1)then
			num_of_u_nod_z=num_of_u_nod_z+1
		endif
	enddo

	if(allocated(obj%u_nod_z) ) deallocate(obj%u_nod_z)
	if(allocated(obj%u_nod_dis_z) ) deallocate(obj%u_nod_dis_z)
	allocate(obj%u_nod_z(num_of_u_nod_z) )
	allocate(obj%u_nod_dis_z(num_of_u_nod_z) )

	num_of_u_nod_z=0
	do i=1,size(obj%femdomain1%boundary%DBoundNodID,1)
		if(obj%femdomain1%boundary%DBoundNodID(i,3) >=1)then
			num_of_u_nod_z=num_of_u_nod_z+1
			obj%u_nod_z(num_of_u_nod_z) = obj%femdomain1%boundary%DBoundNodID(i,3)
			obj%u_nod_dis_z(num_of_u_nod_z) = obj%femdomain1%boundary%DBoundVal(i,3)
		endif
	enddo

	do i=1,size(obj%femdomain2%boundary%DBoundNodID,1)
		if(obj%femdomain2%boundary%DBoundNodID(i,3) >=1)then
			num_of_u_nod_z=num_of_u_nod_z+1
			obj%u_nod_z(num_of_u_nod_z) = obj%femdomain2%boundary%DBoundNodID(i,3)
			obj%u_nod_dis_z(num_of_u_nod_z) = obj%femdomain2%boundary%DBoundVal(i,3)
		endif
	enddo
end subroutine
! #########################################################



! #########################################################
subroutine ls_add_duCM(obj)
	class(ContactMechanics_),intent(inout) :: obj
	integer(int32) :: i
	if(.not. allocated(obj%du_nod_dis_x) )then
		obj%du_nod_dis_x = obj%u_nod_dis_x
	endif
	if(.not. allocated(obj%du_nod_dis_y) )then
		obj%du_nod_dis_y = obj%u_nod_dis_y
	endif
	! regacy subroutine for lodging simulator 2.5
	! this will be revised.
	do i=1,size(obj%u_nod_x,1)
		obj%u_nod_dis_x(i)=obj%du_nod_dis_x(i)
	enddo
	
	do i=1,size(obj%u_nod_y,1)
		obj%u_nod_dis_y(i)=obj%du_nod_dis_y(i)
	enddo
	if(allocated(obj%u_nod_z) )then
		do i=1,size(obj%u_nod_z,1)
			obj%u_nod_dis_z(i)=obj%du_nod_dis_z(i)
		enddo
	endif

end subroutine
! #########################################################


! #########################################################
subroutine getTracBoundCM(obj,dim_num)
	class(ContactMechanics_),intent(inout) :: obj
	integer(int32),optional,intent(in) :: dim_num
	integer(int32) :: i,n,domain1_node_num,node_id,dimnum

	dimnum = input(default=size(obj%femdomain1%mesh%nodcoord,2),option=dim_num)
	! for x
	do i=1,size(obj%femdomain1%boundary%NBoundNodID,1)
		if(obj%femdomain1%boundary%NBoundNodID(i,1) >=1)then
			node_id =obj%femdomain1%boundary%NBoundNodID(i,1) 
			obj%fvec( (node_id-1)*dimnum+1 ) = obj%femdomain1%boundary%NBoundVal(i,1)
		endif
	enddo
	domain1_node_num = size(obj%femdomain1%boundary%NBoundNodID,1)

	do i=1,size(obj%femdomain2%boundary%NBoundNodID,1)
		if(obj%femdomain2%boundary%NBoundNodID(i,1) >=1)then
			node_id =obj%femdomain2%boundary%NBoundNodID(i,1) 
			obj%fvec( (node_id-1)*dimnum+1+domain1_node_num ) &
				= obj%femdomain2%boundary%NBoundVal(i,1)
		endif
	enddo

	! for y
	do i=1,size(obj%femdomain1%boundary%NBoundNodID,1)
		if(obj%femdomain1%boundary%NBoundNodID(i,2) >=1)then
			node_id =obj%femdomain1%boundary%NBoundNodID(i,2) 
			obj%fvec( (node_id-1)*dimnum+2 ) = obj%femdomain1%boundary%NBoundVal(i,2)
		endif
	enddo
	domain1_node_num = size(obj%femdomain1%boundary%NBoundNodID,1)

	do i=1,size(obj%femdomain2%boundary%NBoundNodID,1)
		if(obj%femdomain2%boundary%NBoundNodID(i,2) >=1)then
			node_id =obj%femdomain2%boundary%NBoundNodID(i,2) 
			obj%fvec( (node_id-1)*dimnum+2+domain1_node_num ) &
				= obj%femdomain2%boundary%NBoundVal(i,2)
		endif
	enddo

	if(size(obj%femdomain1%mesh%nodcoord,2)<=2)then
		return
	endif

	! for z
	do i=1,size(obj%femdomain1%boundary%NBoundNodID,1)
		if(obj%femdomain1%boundary%NBoundNodID(i,3) >=1)then
			node_id =obj%femdomain1%boundary%NBoundNodID(i,3) 
			obj%fvec( (node_id-1)*dimnum+2 ) = obj%femdomain1%boundary%NBoundVal(i,3)
		endif
	enddo
	domain1_node_num = size(obj%femdomain1%boundary%NBoundNodID,1)

	do i=1,size(obj%femdomain2%boundary%NBoundNodID,1)
		if(obj%femdomain2%boundary%NBoundNodID(i,3) >=1)then
			node_id =obj%femdomain2%boundary%NBoundNodID(i,3) 
			obj%fvec( (node_id-1)*dimnum+2+domain1_node_num ) &
				= obj%femdomain2%boundary%NBoundVal(i,3)
		endif
	enddo
end subroutine
! #########################################################

! regacy
!-------------------------------
   subroutine displace_nr(Kmat, rvec, u_nod_x, u_nod_dis_x,u_nod_y, u_nod_dis_y)
       integer, intent(in) :: u_nod_x(:), u_nod_y(:)
       real(8), intent(inout) :: Kmat(:,:), rvec(:)
       real(8), intent(in) :: u_nod_dis_x(:), u_nod_dis_y(:)
       integer i,k
	   
	   !Kmat�̕␳
	   do i=1, size(u_nod_x)

	      do k=1, size(kmat,1)
		     kmat(k,2*u_nod_x(i)-1)=0.0d0
			 kmat(2*u_nod_x(i)-1,k)=0.0d0
		  enddo
	   enddo
	   do i=1, size(u_nod_y)

	      do k=1, size(kmat,1)
		     kmat(k,2*u_nod_y(i))=0.0d0
			 kmat(2*u_nod_y(i),k)=0.0d0
		  enddo
	   enddo
	   !�ψʋ��E��̓���
	   do i=1,size(u_nod_x)

	      kmat(2*u_nod_x(i)-1,2*u_nod_x(i)-1)=1.0d0
		  rvec(2*u_nod_x(i)-1)=0.0d0
	   enddo
	   do i=1,size(u_nod_y)

	      kmat(2*u_nod_y(i),2*u_nod_y(i))=1.0d0
		  rvec(2*u_nod_y(i))=0.0d0
	   enddo

	   
   end subroutine displace_nr
!=================================================================================


   subroutine displace(Kmat, Bvec, u_nod_x, u_nod_dis_x,u_nod_y, u_nod_dis_y)
	integer, intent(in) :: u_nod_x(:), u_nod_y(:)
	real(8), intent(inout) :: Kmat(:,:), Bvec(:)
	real(8), intent(in) :: u_nod_dis_x(:), u_nod_dis_y(:)
	integer i,k
	!�O�̓x�N�g���̕␳
	do i=1, size(u_nod_x,1)

	   do k=1,size(kmat,1)
		  Bvec(k)=Bvec(k)-kmat(k,2*u_nod_x(i)-1)/kmat(2*u_nod_x(i)-1,2*u_nod_x(i)-1)*&
		  u_nod_dis_x(i)
	   enddo
	enddo
	do i=1, size(u_nod_y,1)

	   do k=1, size(kmat,1)
		  Bvec(k)=Bvec(k)-kmat(k,2*u_nod_y(i))/kmat(2*u_nod_y(i),2*u_nod_y(i))*&
		  u_nod_dis_y(i)
	   enddo
	enddo
	!Kmat�̕␳
	do i=1, size(u_nod_x)

	   do k=1, size(kmat,1)
		  kmat(k,2*u_nod_x(i)-1)=0.0d0
		  kmat(2*u_nod_x(i)-1,k)=0.0d0
	   enddo
	enddo
	do i=1, size(u_nod_y)

	   do k=1, size(kmat,1)
		  kmat(k,2*u_nod_y(i))=0.0d0
		  kmat(2*u_nod_y(i),k)=0.0d0
	   enddo
	enddo
	!�ψʋ��E��̓���
	do i=1,size(u_nod_x)

	   kmat(2*u_nod_x(i)-1,2*u_nod_x(i)-1)=1.0d0
	   bvec(2*u_nod_x(i)-1)=u_nod_dis_x(i)
	enddo
	do i=1,size(u_nod_y)

	   kmat(2*u_nod_y(i),2*u_nod_y(i))=1.0d0
	   bvec(2*u_nod_y(i))=u_nod_dis_y(i)
	enddo



end subroutine displace

subroutine showPropertyCM(obj)
	class(ContactMechanics_),intent(in) :: Obj
	integer(int32) :: i

	if(allocated(obj%YoungModulus) )then
		do i=1,size(obj%femdomains)
			print *, "Domain-ID ::",i,"YoungModulus ::",obj%YoungModulus(i),"PoissonRatio",obj%PoissonRatio(i),"Density",obj%Density(i)
		enddo
	endif

end subroutine

subroutine setYoungModulus(obj,YoungModulus,DomainID)
	class(ContactMechanics_),intent(inout) :: Obj
	real(real64),intent(in) :: YoungModulus
	integer(int32),optional,intent(in) :: DomainID

	if(present(DomainID) )then
		obj%YoungModulus(DomainID) = YoungModulus
	else
		obj%YoungModulus(:) = YoungModulus
	endif

end subroutine

subroutine setPoissonRatio(obj,PoissonRatio,DomainID)
	class(ContactMechanics_),intent(inout) :: Obj
	real(real64),intent(in) :: PoissonRatio
	integer(int32),optional,intent(in) :: DomainID

	if(present(DomainID) )then
		obj%PoissonRatio(DomainID) = PoissonRatio
	else
		obj%PoissonRatio(:) = PoissonRatio
	endif

end subroutine

subroutine setDensity(obj,density,DomainID)
	class(ContactMechanics_),intent(inout) :: Obj
	real(real64),intent(in) :: density
	integer(int32),optional,intent(in) :: DomainID

	if(present(DomainID) )then
		obj%density(DomainID) = density
	else
		obj%density(:) = density
	endif

end subroutine


subroutine removeContactMechanics(obj)
	class(ContactMechanics_),intent(inout) :: obj


		! Modern 
	if(allocated(obj%FEMDomains)) deallocate(obj%FEMDomains)
	call obj%solver%init()
	if(allocated(obj% contactlist)) deallocate(obj% contactlist)
	if(allocated(obj%YoungModulus)) deallocate(obj%YoungModulus )
	if(allocated(obj%PoissonRatio)) deallocate(obj%PoissonRatio )
	if(allocated(obj%Density)) deallocate(obj%Density )

	if(allocated(obj%YoungModulusList)) deallocate(obj%YoungModulusList )
	if(allocated(obj%PoissonRatioList)) deallocate(obj%PoissonRatioList )
	if(allocated(obj%DensityList)) deallocate(obj%DensityList )

	obj%initialized = .false.

	obj%gravity(1:3) =[0.0d0, 0.0d0, -9.810d0]
	
	obj%penalty = 100000.0d0

	! >>>>>>>>>>>> Regacy >>>>>>>>>>>>>>>>>
	! >>>>>>>>>>>> Regacy >>>>>>>>>>>>>>>>>
	! >>>>>>>>>>>> Regacy >>>>>>>>>>>>>>>>>
	! >>>>>>>>>>>> Regacy >>>>>>>>>>>>>>>>>
	! 
	if(associated(obj%FEMDomain1)) nullify(obj%FEMDomain1)
	if(associated(obj%FEMDomain2)) nullify(obj%FEMDomain2)

	if(associated(obj%FEMIface)) nullify(obj%FEMIface)
	! common fields
	if(allocated(obj% NTSGap)) deallocate(obj%  NTSGap)
	if(allocated(obj% NTSGzi)) deallocate(obj%  NTSGzi)
	obj%penaltypara=dble(1.0e+5)
	obj%FrictionalCoefficient=0.30d0
	obj%Cohesion=0.0d0
	obj%Tolerance=dble(1.0e-10)

	! for weak coupling contact analysis
	if(allocated(obj% Domain1Force)) deallocate(obj%  Domain1Force)
	if(allocated(obj% Domain2Force)) deallocate(obj%  Domain2Force)


	! for strong coupling contact analysys
	if(allocated(obj%KcontactEBE)) deallocate(obj% KcontactEBE)
	if(allocated(obj%KcontactGlo)) deallocate(obj% KcontactGlo)
	if(allocated(obj%FcontactEBE)) deallocate(obj% FcontactEBE)
	if(allocated(obj%FcontactGlo)) deallocate(obj% FcontactGlo)
	if(allocated(obj%DispVecEBE)) deallocate(obj% DispVecEBE)
	if(allocated(obj%DispVecGlo)) deallocate(obj% DispVecGlo)
	if(allocated(obj%NTSvariables)) deallocate(obj% NTSvariables)
	if(allocated(obj%ContactMatPara)) deallocate(obj% ContactMatPara)
	if(allocated(obj%GloNodCoord)) deallocate(obj% GloNodCoord)
	
	! boundary conditions for lodging simulator 2.5
	if(allocated(obj%u_nod_x)) deallocate(obj%u_nod_x)
	if(allocated(obj%u_nod_y)) deallocate(obj%u_nod_y)
	if(allocated(obj%u_nod_z)) deallocate(obj%u_nod_z)
	if(allocated(obj%du_nod_dis_x)) deallocate(obj% du_nod_dis_x)
	if(allocated(obj%du_nod_dis_y)) deallocate(obj% du_nod_dis_y)
	if(allocated(obj%du_nod_dis_z)) deallocate(obj% du_nod_dis_z)
	if(allocated(obj%u_nod_dis_x)) deallocate(obj% u_nod_dis_x)
	if(allocated(obj%u_nod_dis_y)) deallocate(obj% u_nod_dis_y)
	if(allocated(obj%u_nod_dis_z)) deallocate(obj% u_nod_dis_z)
	if(allocated(obj% duvec)) deallocate(obj%  duvec)
	if(allocated(obj%  uvec)) deallocate(obj%   uvec)
	if(allocated(obj% dfvec)) deallocate(obj%  dfvec)
	if(allocated(obj%  fvec)) deallocate(obj%   fvec)

	if(allocated(obj%NTSMaterial)) deallocate(obj%NTSMaterial)
	if(allocated(obj%StickOrSlip)) deallocate(obj%StickOrSlip)

	obj%step=0
	obj%itr_contact=0
	obj%itr=0
	obj%BiCG_ItrMax=10000
	obj%NR_ItrMax=100
	obj%control=1 ! 1:displacement-control, 2: traction-control
	obj%TimeStep=100

	! from lodging-simulatiro 2.5

	if(allocated(obj%nts_elem_nod)) deallocate(obj%nts_elem_nod)
	if(allocated(obj%old_nts_elem_nod)) deallocate(obj%old_nts_elem_nod)
	if(allocated(obj%surface_nod)) deallocate(obj%surface_nod)
	if(allocated(obj%sur_nod_inf)) deallocate(obj%sur_nod_inf)
	if(allocated(obj%nod_coord)) deallocate(obj% nod_coord)
	if(allocated(obj%old_nod_coord)) deallocate(obj% old_nod_coord)
	if(allocated(obj%elem_nod)) deallocate(obj% elem_nod)
	if(allocated(obj% nts_mat)) deallocate(obj% nts_mat)
	if(allocated(obj% sur_inf_mat)) deallocate(obj% sur_inf_mat)
	if(allocated(obj% contact_mat)) deallocate(obj% contact_mat)
	if(allocated(obj%contact_mat_para)) deallocate(obj% contact_mat_para)
	if(allocated(obj% active_nts)) deallocate(obj% active_nts)

	if(allocated(obj%k_contact)) deallocate(obj% k_contact)
	if(allocated(obj%fvec_contact)) deallocate(obj% fvec_contact)
	if(allocated(obj%nts_amo)) deallocate(obj% nts_amo)
	if(allocated(obj% stick_slip)) deallocate(obj% stick_slip)
	if(allocated(obj% old_stick_slip)) deallocate(obj% old_stick_slip)
	if(allocated(obj%old_nts_amo)) deallocate(obj% old_nts_amo)
	if(allocated(obj%kmat)) deallocate(obj% kmat)
	if(allocated(obj%gvec)) deallocate(obj% gvec)
	if(allocated(obj%rvec)) deallocate(obj% rvec)
	if(allocated(obj%K_total)) deallocate(obj% K_total)
	if(allocated(obj%initial_duvec)) deallocate(obj% initial_duvec)
	if(allocated(obj%dduvec)) deallocate(obj% dduvec)
	if(allocated(obj%dduvec_nr)) deallocate(obj% dduvec_nr)



end subroutine

subroutine solveCM(obj,Algorithm)
	class(ContactMechanics_),target,intent(inout) :: obj
	character(*),intent(in) :: Algorithm
	!integer(int32),intent(in) :: domainID
	!type(FEMDomain_),pointer :: a_domain
	
	call obj%solver%solve(Algorithm)

	!a_domain => obj%femdomains(domainID)%femdomainp
	!! update displacement
	obj%displacement = obj%solver%x
!
	!! udate traction force
	!obj%TractionForce = reshape(a_domain%TractionVector(&
	!	displacement=obj%displacement,&
	!	YoungModulus=obj%YoungModulus,&
	!	PoissonRatio=obj%PoissonRatio) ,a_domain%nn(),a_domain%nd() )

end subroutine

end module 