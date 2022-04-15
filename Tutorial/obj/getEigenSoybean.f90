use SoybeanClass
implicit none


type(Soybean_) :: soy
real(real64),allocatable :: displacement(:),a(:),Modes(:,:), Freq(:)
type(IO_) :: f
type(Math_) :: math

call soy%init("Tutorial/obj/mini_soy.json")
call soy%remove(root=.true.)
call soy%vtk("soy",single_file=.true.)


call soy%checkMemoryRequirement()
    
call soy%setYoungModulus(100000.0d0,stem=.true.)
call soy%setYoungModulus(10000.0d0,leaf=.true.)
call soy%setPoissonRatio(0.30d0)
call soy%setDensity(1.200d0)

deallocate(soy%leaf)
do i_i=1,size(soy%stem2stem,1)
    do j_j=1,size(soy%stem2stem,1)
        if(soy%stem2stem(i_i,j_j)/=0 )then
            print *, i_i,j_j,soy%stem2stem(i_i,j_j)
        endif
    enddo
enddo


Modes = soy%getEigenMode(ground_level=0.00d0,penalty=1000000000.0d0,&
    debug=.true., Frequency=Freq, EbOM_Algorithm="P2P")


! fundamental
displacement = Modes(:,1)

print *, maxval(displacement),minval(displacement)

call f%open("ModeVectors.txt")
call f%write(displacement)
call f%close()

print *, freq(1:30)

do i_i = 1, 20
    do j_j = 1, 100
        displacement = Modes(:,i_i)/10.0d0*cos(dble(j_j)/100.0d0*2.0d0*math%pi )
        call soy%deform(displacement =   displacement)
        call soy%vtk("soy_deform_"+str(i_i)+"step_"+str(j_j),single_file = .true.)
        call soy%deform(displacement = - displacement)
    enddo
enddo

end 