use LeafClass
implicit None

type(Leaf_) :: leaf
real(real64),allocatable :: ny(:),n(:)

call leaf%init()
call leaf%vtk("sample_leaf")


print *, leaf%femdomain%mesh%nodcoord(&
    leaf%A_PointNodeID,:)

print *, leaf%femdomain%mesh%nodcoord(&
    leaf%C_PointNodeID,:)


print *, leaf%femdomain%mesh%nodcoord(&
    leaf%B_PointNodeID,:)
    
print *, leaf%femdomain%mesh%nodcoord(&
    leaf%D_PointNodeID,:)

print *, "--"
print *, leaf%femdomain%centerPosition(ElementID=leaf%A_PointElementID)
print *, leaf%femdomain%centerPosition(ElementID=leaf%C_PointElementID)
print *, leaf%femdomain%centerPosition(ElementID=leaf%B_PointElementID)
print *, leaf%femdomain%centerPosition(ElementID=leaf%D_PointElementID)

print *, "nomal"
print *, leaf%femdomain%ne()
ny = zeros(leaf%femdomain%ne())
do j_j=1,90
    call leaf%rotate(x=radian(-1.0d0) )
    do i_i=1,leaf%femdomain%ne()
        n = leaf%getNormalVector(ElementID=i_i)
        ny(i_i) = n(2)
    enddo
    call leaf%femdomain%vtk("sample_leaf_ny"+str(j_j),scalar=ny)
enddo

end
