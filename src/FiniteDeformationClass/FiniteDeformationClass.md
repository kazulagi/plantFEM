### Mannual for the Finite Deformation Class

## obj%DeformStrain

- 2D cases:

obj%DeformStrain(:,:,1) = Cp_iJ_n(1,1)
obj%DeformStrain(:,:,2) = Cp_iJ_n(2,2)
obj%DeformStrain(:,:,3) = Cp_iJ_n(1,2)
obj%DeformStrain(:,:,4) = Cp_iJ(1,1)
obj%DeformStrain(:,:,5) = Cp_iJ(2,2)
obj%DeformStrain(:,:,6) = Cp_iJ(2,1)
obj%DeformStrain(:,:,7) = F_iJ_n(1,1)
obj%DeformStrain(:,:,8) = F_iJ_n(2,2)
obj%DeformStrain(:,:,9) = F_iJ_n(1,2)
obj%DeformStrain(:,:,10)= F_iJ_n(2,1)
obj%DeformStrain(:,:,11)= F_iJ(1,1)
obj%DeformStrain(:,:,12)= F_iJ(2,2)
obj%DeformStrain(:,:,13)= F_iJ(1,2)
obj%DeformStrain(:,:,14)= F_iJ(2,1)
obj%DeformStrain(:,:,15)= None

- 3D cases:

obj%DeformStrain(:,:,1) = Cp_iJ_n(1,1)
obj%DeformStrain(:,:,2) = Cp_iJ_n(2,2)
obj%DeformStrain(:,:,3) = Cp_iJ_n(1,2)
obj%DeformStrain(:,:,4) = Cp_iJ(1,1)
obj%DeformStrain(:,:,5) = Cp_iJ(2,2)
obj%DeformStrain(:,:,6) = Cp_iJ(2,1)
obj%DeformStrain(:,:,7) = F_iJ_n(1,1)
obj%DeformStrain(:,:,8) = F_iJ_n(2,2)
obj%DeformStrain(:,:,9) = F_iJ_n(1,2)
obj%DeformStrain(:,:,10)= F_iJ_n(2,1)
obj%DeformStrain(:,:,11)= F_iJ(1,1)
obj%DeformStrain(:,:,12)= F_iJ(2,2)
obj%DeformStrain(:,:,13)= F_iJ(1,2)
obj%DeformStrain(:,:,14)= F_iJ(2,1)
obj%DeformStrain(:,:,15)= None

obj%DeformStrain(:,:,16) = Cp_iJ_n(3,3)
obj%DeformStrain(:,:,17) = Cp_iJ_n(1,3)
obj%DeformStrain(:,:,18) = Cp_iJ_n(2,3)

obj%DeformStrain(:,:,19) = Cp_iJ(3,3)
obj%DeformStrain(:,:,20) = Cp_iJ(1,3)
obj%DeformStrain(:,:,21) = Cp_iJ(2,3)

obj%DeformStrain(:,:,22) = F_iJ_n(3,3)
obj%DeformStrain(:,:,23) = F_iJ_n(1,3)
obj%DeformStrain(:,:,24) = F_iJ_n(2,3)
obj%DeformStrain(:,:,25) = F_iJ_n(3,1)
obj%DeformStrain(:,:,26) = F_iJ_n(3,2)

obj%DeformStrain(:,:,27)= F_iJ(3,3)
obj%DeformStrain(:,:,28)= F_iJ(1,3)
obj%DeformStrain(:,:,29)= F_iJ(2,3)
obj%DeformStrain(:,:,30)= F_iJ(3,1)
obj%DeformStrain(:,:,31)= F_iJ(3,2)

obj%DeformStrain(:,:,32)= None