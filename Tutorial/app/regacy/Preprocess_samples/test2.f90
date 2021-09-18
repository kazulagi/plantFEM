program main
    use SiCroF

    type(FEMDomain_)::obj

    call obj%import(OptionalProjectName="finitedtest1")
    call obj%move(x=10.0d0,y=0.0d0,z=0.0d0)
    !call obj%rotate(x=0.0d0,y=3.14159d0,z=0.0d0)
    print *, obj%FileName
    call obj%GmshPlotMesh(Name="hogehoge",withNeumannBC=.true.,withDirichletBC=.true.)
    call obj%export(OptionalProjectName="finitedtest3")
    
end program main