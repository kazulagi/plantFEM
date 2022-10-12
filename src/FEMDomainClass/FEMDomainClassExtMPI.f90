!##################################################
subroutine distributeFEMDomain(obj,mpid) 
	use MPIClass
	use FEMDOmainClass
	class(FEMDomain_),intent(inout)::obj
    type(Mesh_),allocatable :: meshes(:)
	type(MPI_),intent(inout) :: mpid
	integer(int32) :: n
	
	n=mpid%petot

	! split obj into n objects
	!if(allocated(obj%FEMDomains) )then
	!	deallocate(obj%FEMDomains)
	!endif
	!allocate(obj%FEMDomains(n))

	! Greedy algorithm
	if(obj%Mesh%empty() .eqv. .true. )then
		print *, "distributeFEMDomain >> ERROR >> No mesh is imported."
		stop
	endif
	
	meshes = obj%mesh%divide(n)

	! import mesh
	call obj%import(Mesh=meshes(mpid%myrank+1))


end subroutine distributeFEMDomain
!##################################################
