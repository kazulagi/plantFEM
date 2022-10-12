use MaizeClass
implicit none

type(Maize_) :: maize(1:10)


do i_i=1,10
    call maize(i_i)%create(config="Tutorial/obj/realMaizeConfig.json")
    call maize(i_i)%move(x=i_i*0.20d0,y = 1.00d0)
    call maize(i_i)%vtk("maize"+zfill(i_i,3),single_file=.true.)
enddo

end