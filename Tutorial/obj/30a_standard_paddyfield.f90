use CivilItemClass
implicit none

type(FEMDomain_) :: paddyfield(10),paddyfield_2(10),channel(3)
type(CivilItem_) :: ci
integer(int32)   :: i

! simple paddyfield
channel(1) = ci%OpenChannel(&
    Length=300.0d0,&
    Width=10.0d0,&
    Depth=10.0d0,&
    ChannelWidth=2.0d0,&
    ChannelDepth=2.0d0,&
    SlopeAngle  =45.0d0,&
    SlopeDepth =1.0d0,&
    refine_level=[2,2,2])
call channel(1)%rotate(z=radian(90.0d0))
call channel(1)%move(x=-55.0d0,y=4.5d0*30.0d0)
call channel(1)%vtk("channel"+zfill(1,4) )

channel(2) = ci%OpenChannel(&
    Length=300.0d0,&
    Width=1.0d0,&
    Depth=10.40d0,&
    ChannelWidth=0.40d0,&
    ChannelDepth=0.40d0,&
    refine_level=[2,2,2])
call channel(2)%rotate(z=radian(90.0d0))
call channel(2)%move(x=50.0d0+0.50d0,y=4.5d0*30.0d0,z=0.40d0)
call channel(2)%vtk("channel"+zfill(2,4) )


channel(3) = ci%OpenChannel(&
    Length=300.0d0,&
    Width=1.0d0,&
    Depth=10.40d0,&
    ChannelWidth=0.40d0,&
    ChannelDepth=0.40d0,&
    refine_level=[2,2,2])
call channel(3)%rotate(z=radian(90.0d0))
call channel(3)%move(x=-150.0d0-0.50d0-10.0d0,y=4.5d0*30.0d0,z=0.40d0)
call channel(3)%vtk("channel"+zfill(3,4) )


paddyfield(1) = ci%PaddyField(&
    Length=100.0d0,&
    Width=30.0d0,&
    Depth=10.0d0,&
    RidgeWidth=1.0d0,&
    RidgeHeight=0.20d0,&
    refine_level=[5,4,5])

do i=2,size(paddyfield)
    paddyfield(i) = paddyfield(1)
    call paddyfield(i)%move(y=30.0d0*(i-1)  )
enddo

paddyfield_2 = paddyfield

do i=1,size(paddyfield_2)
    call paddyfield_2(i)%move(x=-110.0d0  )
enddo



do i=1,size(paddyfield)
    call paddyfield(i)%vtk("paddyfield"//zfill(i,4) )
enddo

do i=1,size(paddyfield_2)
    call paddyfield_2(i)%vtk("paddyfield"//zfill(i+10,4) )
enddo

end