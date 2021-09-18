program main
    use FarmClass
    implicit none

    type(Farm_):: SoybeanField

    call SoybeanField%sowing(crop_name="soybean",single=.false.,&
        width_of_farm=100.0d0*100.0d0, length_of_farm=200.0d0*100.0d0)
    call SoybeanField%fertilize(&
        N_kg=30.0d0,P_kg=100.0d0,K_kg=100.0d0)
    call SoybeanField%export(filename="../soybean")
    call SoybeanField%diagnosis()

end program 