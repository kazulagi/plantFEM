program main
    use PreprocessingClass
    implicit none

    type(mpi_)::mpid
    type(Mesh_) :: rootmesh
    type(Preprocessing_)::root, soil
    integer(int32) :: i,n,m
    
    call mpid%start()
    call rootmesh%create(meshtype="RootAndSoil2D",&
        x_num=10,y_num=100,x_len=5.0d0,y_len=50.0d0,Lh=10.0d0,Le=5.0d0)
    call root%import(mesh=rootmesh)

    call Soil%SetEntity(Rectangle=.true.,Xsize=100.0d0, Ysize=70.0d0,xloc=-50.0d0,yloc=-75.0d0)
    call Soil%Boolean(root)
    call Soil%ExportGeoFile(mpid,Name="Soil")
    call Soil%ConvertGeo2Mesh(mpid,Name="Soil",SizePara=10)
    call Soil%ConvertMesh2Scf(mpid,ElementType="LinearRectangularGp4",Name="Soil.mesh")
    
    call Soil%Convert3Dto2D()
    call root%showMesh(Name="root")
    call soil%showMesh(Name="soil")

    print *, "Root mesh check"
    call root%FEMDomain%Mesh%check()
    print *, "Root mesh check"
    call soil%FEMDomain%Mesh%check()

    call showArray(rootmesh%ElemNod)
    call showArraySize(rootmesh%NodCoord)
    call showArraySize(rootmesh%ElemNod)

    ! export as LS25 file
    call root%ExportAsLodgingSim(soil,Name="input_for_ls25.scf")

    call mpid%end()
end program main