program create_soybean
    use plantFEM
    implicit none
    
    type(Soybean_) :: soybean
    character(200) :: in_name,out_name

    call getarg(1, in_name)
    call getarg(2,out_name)
    call soybean%create(trim(in_name))
    call soybean%vtk(trim(out_name),single_file=True)

end program