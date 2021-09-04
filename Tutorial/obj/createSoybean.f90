use SoybeanClass
implicit none

type(Soybean_) :: soy
real(real64) :: biomass,totalweight

call soy%init(config="Tutorial/obj/realSoybeanConfig.json")


biomass = soy%getBiomass(stem=true)
print *, "Total Dry Weight (stem) : ",biomass*1000.0d0," g"
biomass = soy%getBiomass(leaf=true)
print *, "Total Dry Weight (leaf) : ",biomass*1000.0d0," g"
biomass = soy%getBiomass(leaf=true,stem=true)
print *, "Total Dry Weight (leaf+stem) :/0.20 m^2 ",biomass*1000.0d0*9.50d0," g/m^2"


totalweight = soy%gettotalweight(stem=true)
print *, "Total Dry Weight (stem) : ",totalweight*1000.0d0," g"
totalweight = soy%gettotalweight(leaf=true)
print *, "Total Dry Weight (leaf) : ",totalweight*1000.0d0," g"
totalweight = soy%gettotalweight(leaf=true,stem=true)
print *, "Total Dry Weight (leaf+stem) :/0.20 m^2 ",totalweight*1000.0d0*9.50d0," g/m^2"


print *, "Stem Volume (m^3):",soy%getVolume(stem=true)
print *, "Stem Volume (cm^3):",soy%getVolume(stem=true)*100.0d0*100.0d0*100.0d0
print *, "Leaf Volume (m^3):",soy%getVolume(leaf=true)
print *, "Leaf Volume (cm^3):",soy%getVolume(leaf=true)*100.0d0*100.0d0*100.0d0
!call soy%msh(name="soy")

end