program create_wheat
    use plantFEM
    implicit none
    
    type(wheat_) :: wheat
    character(200) :: in_name,out_name

    call getarg(1, in_name)
    call getarg(2,out_name)
    call wheat%create(trim(in_name))
    call wheat%vtk(trim(out_name),single_file=True)

end program