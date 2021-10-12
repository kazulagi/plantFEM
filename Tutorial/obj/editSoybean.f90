use SoybeanClass
implicit none

type(Soybean_)   :: soy
type(FEMDomain_) :: domain
integer(int32)   :: i

call soy%init(config="Tutorial/obj/realSoybeanConfig.json") 
! check properties
print *, "Numer of sub-domains :: ", soy%ns()
print *, "Numer of Elements :: ", soy%ne()
print *, "Numer of points :: ", soy%np()

! change scale of each subdomain
call soy%stl("Before")
do i=1,soy%ns()
    domain = soy%getSubDomain(ID=100)
    call domain%fat(ratio=0.030d0)
    call soy%setSubDomain(ID=100,Domain=domain)
enddo
call soy%stl("After")



end