program main
    use FarmClass
    implicit none

    type(Farm_):: SoybeanField
    
    call SoybeanField%sowing(crop_name="soybean",single=.false.,variety="Tachinagaha")
    call SoybeanField%export(FileName="/home/haruka/test/soybean")

end program 