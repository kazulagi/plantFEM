use LeafClass

implicit none

type(Leaf_) :: leaf

call leaf%create(filename="Tutorial/obj/grape_leaf.txt")
call leaf%vtk("grape")

end