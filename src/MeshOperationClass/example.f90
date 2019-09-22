program main
    use FEMDomainClass
    use FEMIfaceClass
    use DiffusionEquationClass
    use FiniteDeformationClass
    use MultiPhysicsIfaceClass
    use PostProcessingClass
    implicit none

    type(FEMDomain_),target::rootdomain1,rootdomain2
    type(FEMIface_ ),target::iface12
    type(DiffusionEq_) ::field1
    type(FiniteDeform_)::field2
    
    character*70 :: InProjName1,InProjName2,gmsh,GaussJordan,Deformation,BiCGSTAB
    integer :: i,j,Step
    real(8) :: time

    gmsh="Gmsh"
    GaussJordan="GaussJordan"
    BiCGSTAB="BiCGSTAB"
    Deformation="Deformation"
    
    ! ###### Read Infile info ###################
    print *, "Import Project Name (Diffusion) is : "
    !read *, InProjName1
    InProjName1="difftest3d"
    print *, trim(InProjName1),".scf"
    print *, "Import Project Name (Deformation) is : "
    !read *, InProjName2
    InProjName2="finitedtest"
    print *, trim(InProjName2),".scf"
    print *, "Time duration (sec.): "
    read *, time
    ! ###### Read Infile info ###################
    
    ! ###### Import Objects ###################
    call ImportFEMDomainDiff(rootdomain1,OptionalProjectName=InProjName1)
    field1%FEMDomain => rootdomain1
    field1%dt=time/dble(field1%FEMDomain%ControlPara%Timestep)
    
    call ImportFEMDomainFiDe(rootdomain2,OptionalProjectName=InProjName2)
    field2%FEMDomain => rootdomain2
    field2%dt=time/dble(field2%FEMDomain%ControlPara%Timestep)
    ! ###### Import Objects ###################



    rootdomain1%Mesh%NodCoord(:,1:2)=rootdomain1%Mesh%NodCoord(:,1:2)+3.0d0

    call GetFEMIface(rootdomain1,rootdomain2,iface12)    
    
end program