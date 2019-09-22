
call MergeMesh(inobj1%Mesh,inobj2%Mesh,outobj%Mesh)
call MergeMaterialProp(inobj1%MaterialProp,inobj2%MaterialProp,outobj%MaterialProp)
call MergeDBound(inobj1%Boundary,inobj1%Mesh,inobj2%Boundary,inobj2%Mesh,outobj%Boundary)
call MergeNBound(inobj1%Boundary,inobj1%Mesh,inobj2%Boundary,inobj2%Mesh,outobj%Boundary)