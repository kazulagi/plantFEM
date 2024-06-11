program create_rice
    use plantFEM
    implicit none
    
    type(rice_) :: rice
    character(200) :: in_name,out_name

    call getarg(1, in_name)
    call getarg(2,out_name)
    call rice%create(trim(in_name))
    call rice%vtk(trim(out_name),single_file=True)

end program