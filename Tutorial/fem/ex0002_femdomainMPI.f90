subroutine distribute(obj,mpid) 
	use MPIClass
	use FEMDOmainClass
	type(FEMDomain_),intent(inout)::obj
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


end subroutine distribute


program main
    use fem
    implicit none

    type(FEMDomain_) :: domain
    type(MPI_) :: mpid

    call mpid%start()
    
    call domain%create(meshtype="rectangular3D",x_num=5,y_num=5,z_num=10,&
        x_len=5.0d0,y_len=5.0d0,z_len=10.0d0)
        
    call distribute(domain,mpid)

    call mpid%end()
end program