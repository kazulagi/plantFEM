program sample
    use SeedClass
    implicit none

    type(Seed_) :: dry_seed,wet_seed

    call dry_seed%init(radius=2.0d0,width1=9.70d0,width2=8.20d0,width3=7.70d0)
    !call wet_seed%init(radius=2.0d0,width1=14.0d0,width2=10.0d0,width3=8.70d0)
    call dry_seed%createMesh(FileName="/home/haruka/test/seed_dry",withSTL=.true.,ElemType="Tetrahedra")
    !call wet_seed%createMesh(FileName="/home/haruka/test/seed_wet",withSTL=.true.,ElemType="Tetrahedra")

    
end program