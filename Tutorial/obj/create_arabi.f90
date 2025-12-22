program create_maize
    use ArabidopsisClass
    implicit none
    
    type(Arabidopsis_) :: arabi

    call arabi%create("arabidopsis.json")
    call arabi%vtk("arabi",single_file=True)

end program

