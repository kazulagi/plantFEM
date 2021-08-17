use LeafClass

implicit none

type(Leaf_) :: leaf

call leaf%create(filename="Tutorial/playon_obj/grape_leaf.txt")
call leaf%vtk("grape")

end