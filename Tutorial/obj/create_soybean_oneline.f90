use SoybeanClass
implicit none

type(Soybean_) :: soy
real(real64),allocatable :: displacement(:),Modes(:,:), Freq(:)
real(real64) :: E_value

E_value = 10.0d0*1000.0d0 ! 10 MPa

soy = to_soybean(&
        node_length=[4.0,4.0,5.0,9.50,4.50,3.0]/100.0d0,&
        node_diameter=[5.0,5.0,5.0,5.0,5.0,5.0]/1000.0d0,&
        node_weight_g=[0.142d0,0.142d0,0.188d0,0.462d0,0.213d0,0.099d0],&
        peti_length=[1.0 ,5.0 ,9.50 ,4.50 ,1.0 ,1.0 ]/100.0d0,&
        peti_diameter=[5.0,5.0,5.0,5.0,5.0,5.0]/1000.0d0,&
        leaf_length=[4.0 ,6.0 ,9.50 ,4.50 ,1.0 ]/100.0d0,&
        leaf_width=[3.0 ,4.5 ,7.0 ,4.0 ,0.50 ]/100.0d0,&
        leaf_thickness = 0.0030d0,&
        num_leaf_set=[2,1,1,1,1],&
        num_leaf_per_set=[1,3,3,3,3],&
        leaf_peti_weight_g=[&
                dble(0.235),&
                dble(0.235),& 
                dble(0.241),& 
                dble(0.161+0.222+0.166+0.059),&
                dble(0.148+0.192+0.134+0.089),&
                dble(0.098+0.152+0.099+0.076) &
            ] )

call soy%vtk("08132025",single_file=.true.)

do i_i=1,size(soy%stem)
    print *, i_i, allocated(soy%stem(i_i)%density)
enddo


print *, size(soy%stemDensity),size(soy%leafDensity),soy%ne()
print *, minval(soy%stemDensity),maxval(soy%stemDensity)
print *, minval(soy%leafDensity),maxval(soy%leafDensity)

call soy%checkMemoryRequirement()

call soy%setYoungModulus(E_value,stem=.true.)
call soy%setYoungModulus(E_value,leaf=.true.)
call soy%setPoissonRatio(0.30d0)
! 
Modes = soy%getEigenMode(ground_level=0.020d0,penalty=10000000.0d0,num_mode=3,&
     debug=.true., Frequency=Freq,EbOM_Algorithm = "P2P")
 
! fundamental
displacement = Modes(:,1)
print *, maxval(displacement),minval(displacement)
displacement = displacement * dble(1.0e+7)
call soy%deform(displacement = displacement)
call soy%vtk("soy_deform",single_file = .true.)

end