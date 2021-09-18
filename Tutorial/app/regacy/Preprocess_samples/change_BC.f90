program main
    use PreprocessingClass

    type(Preprocessing_) :: leaf

    call leaf%import(Name="Tutorial/InputData/grass_leaf")
    call leaf%removeBC(Initial=.true.)
    call leaf%SetSizeOfBC(Initial=.true. , NumOfValue=1)
    call leaf%SetBC(Initial=.true.,   val=0.0d0,val_id=1)
    call leaf%Export(Name="Tutorial/InputData/grass_leaf_BC_changed")

end program 
