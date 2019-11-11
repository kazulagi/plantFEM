program sample
    use SeedClass
    implicit none

    type(Seed_) :: dry_seed,wet_seed

    call dry_seed%init(radius=2.0d0,width1=9.70d0,width2=8.20d0,width3=7.70d0)
    call wet_seed%init(radius=2.0d0,width1=14.0d0,width2=10.0d0,width3=8.70d0,x=10.0d0)
    call dry_seed%createMesh(FileName="/home/haruka/test/seed_dry",ElemType="Tetrahedra")
    call wet_seed%createMesh(FileName="/home/haruka/test/seed_wet",ElemType="Tetrahedra")
    call dry_seed%convertMeshType(Option="TetraToHexa")
    call wet_seed%convertMeshType(Option="TetraToHexa")
    call dry_seed%FEMDomain%GmshPlotMesh()
    call wet_seed%FEMDomain%GmshPlotMesh()
    call dry_seed%export(FileName="/home/haruka/test/seed_dry",extention=".stl")
    call wet_seed%export(FileName="/home/haruka/test/seed_wet",extention=".stl")
    
    
end program