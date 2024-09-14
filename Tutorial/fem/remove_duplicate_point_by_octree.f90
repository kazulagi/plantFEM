use FEMDomainClass
implicit none

real(real64),allocatable :: Points(:,:)
integer(int32),allocatable :: PointIdx(:)

Points = zeros(6,3)
Points(1,:) = [1.0d0, 0.0d0, 0.0d0]
Points(2,:) = [0.0d0, 2.0d0, 0.0d0]
Points(3,:) = [0.0d0, 0.0d0, 1.0d0]
Points(4,:) = [1.0d0, 1.0d0, 0.0d0]
Points(5,:) = [1.1d0, 0.0d0, 0.0d0]
Points(6,:) = [0.0d0, 0.0d0, 1.0d0]

PointIdx = [(i_i,i_i=1,size(Points,1))]
print *, OcTreeSearch(Points,PointIdx,MinimumDist=dble(0.000001))

end