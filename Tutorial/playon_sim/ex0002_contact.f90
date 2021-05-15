program main
    use plantFEM
    implicit none

    type(FEMDomain_) :: domain1, domain2, domain3 ! FEM data-structure
    type(IO_) :: f
    integer(int32) :: i

    call domain1%create(meshtype="rectangular3D",x_num=10,y_num=10,z_num=10,&
    x_len=10.0d0,y_len=10.0d0,z_len=10.0d0)
    

    call domain2%create(meshtype="rectangular3D",x_num=10,y_num=10,z_num=10,&
    x_len=10.0d0,y_len=10.0d0,z_len=10.0d0)
    call domain2%move(x=9.50d0)

    call domain3%create(meshtype="rectangular3D",x_num=10,y_num=10,z_num=10,&
    x_len=10.0d0,y_len=10.0d0,z_len=10.0d0)
    call domain3%move(x=9.50d0)
    call domain3%rotate(x=1.0d0,y=1.0d0)

    call domain1%contactdetect(domain2)
    call domain1%contactdetect(domain3)

    ! export

    call domain1%gmsh(name="domain1.txt")
    call domain2%gmsh(name="domain2.txt")
    call domain3%gmsh(name="domain3.txt")

    call savetxt(domain1%Boundary%MasterNodeID,"./","master",".txt")
    call savetxt(domain1%Boundary%SlaveNodeID,"./","slave",".txt")
    
    call f%open("./"//"name"//".txt",'w')
    do i=1,size(domain1%Boundary%ContactNameList,1)
        write(f%fh,*) trim(domain1%Boundary%ContactNameList(i)%Name)
    enddo
    call f%close()
    

    
end program main 
