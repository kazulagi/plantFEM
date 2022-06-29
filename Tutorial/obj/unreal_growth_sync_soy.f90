use SoybeanClass
implicit none

type(Soybean_) :: soy

type(Soybean_) :: soy_opt(1:100)
real(real64)   :: score(1:100)
type(FEMDomain_):: point_cloud
integer(int32) :: i, time_step
integer(int32) :: StemID,InterNodeID,PetioleID,LeafID
real(real64) :: t, L,R,Lmax,L0,phi ! phase delay

soy%max_num_leaf_per_petiole = 3
call soy%init("Tutorial/obj/soy.json")

! Edit soybean
call soy%vtk("soy_before",single_file=.true.)


do time_step = 1, 100
    print *, "day",time_step
    do StemID=0,soy%maxStemID()
        
        do InterNodeID=1,soy%maxInterNodeID(StemID=StemID)

            t = time_step - InterNodeID ! day
            if(t < 0.0d0)then
                t=0.0d0
            endif
            L0 = 0.030d0
            Lmax  = 0.12d0
            R = 1.0d0
            L = Lmax/(1.0d0 + (Lmax/L0 - 1.0d0)*exp(-R*t) )

            call soy%resizeStem(&
            StemID=StemID,& ! main=0, branch=1,2 ...
            InterNodeID=InterNodeID,& ! 1,2,3...
            Length     = L &  ! m , optional
            )

            do PetioleID=1,soy%maxPetioleID(StemID=StemID,InterNodeID=InterNodeID)
                call soy%resizePetiole(&
                StemID=StemID,& ! main=0, branch=1,2 ...
                InterNodeID=InterNodeID,& ! 1,2,3...
                PetioleID = petioleID,&
                Length     = L &  ! m , optional
                )

                do LeafID = 1,soy%maxLeafID( &
                        StemID=StemID,  &
                        InterNodeID=InterNodeID,&
                        PetioleID = PetioleID )

                    call soy%resizeLeaf(&
                        StemID=StemID,& ! main=0, branch=1,2 ...
                        InterNodeID=InterNodeID,& ! 1,2,3...
                        PetioleID = petioleID,&
                        LeafID=LeafID,&
                        Length     = L*2.0d0 ,&  ! m , optional
                        Width     = L & 
                    )
                
                enddo
            enddo



            call soy%rotateStem(&
            StemID=StemID,& ! main=0, branch=1,2 ...
            InterNodeID=InterNodeID,& ! 1,2,3...
            angles = [0.0d0, 0.0d0, 20.0d0] &! [x, y, z] angles (deg.)
            )


            call soy%vtk("soy_after"+str(time_step),single_file=.true.)

        enddo
    enddo
enddo

call soy%vtk("soy_after",single_file=.true.)

end