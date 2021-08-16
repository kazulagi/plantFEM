use GrapeClass
implicit none

type(Grape_) :: grape
type(IO_) :: grapeconfig

call grape%create(config = "Tutorial/playon_obj/realGrapeConfig.json")

print *, grape%mainstem_length
print *, grape%mainstem_width
print *, grape%mainstem_node

print *, grape%num_branch
print *, grape%num_branch_node

print *, grape%num_leaf
print *, grape%num_stem
print *, grape%num_root

end