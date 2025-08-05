program create_maize
    use plantFEM
    implicit none
    
    type(maize_) :: maize
    character(200) :: in_name,out_name

    call getarg(1, in_name)
    call getarg(2,out_name)
    call maize%create(trim(in_name))
    call maize%vtk(trim(out_name),single_file=True)

end program

