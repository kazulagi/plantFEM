
if(allocated(obj%Nmat) ) then
    deallocate(obj%Nmat)
endif
allocate(obj%Nmat(obj%NumOfNode) )


if(obj%NumOfNode==1) then
    !#########################################################################
    !#######                                                           #######
    !#######                             +     (1)                     #######
    !#######                                                           #######
    !#######                                                           #######
    !#########################################################################
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction NumOfNode==1 "
    obj%ierr=1

elseif(obj%NumOfNode==2) then
    
    if(obj%NumOfDim==1) then
        
        !#########################################################################
        !#######                                                           #######
        !#######                +>----------------------->+                #######
        !#######            (1)                               (2)          #######
        !#######                                                           #######
        !#########################################################################
        
        obj%Nmat(1)=0.50d0*( 1.0d0-obj%gzi(1))
        obj%Nmat(2)=0.50d0*(-1.0d0+obj%gzi(1))
        
        obj%ErrorMsg="Succeed::GetShapeFunction "
        obj%ierr=0
    else
        print *, "ERROR::GetShapeFunction"
        obj%ErrorMsg="ERROR::GetShapeFunction NumOfNode==2 NumOfOrder/=1 "
        obj%ierr=1
    endif
elseif(obj%NumOfNode==3) then
    if(obj%NumOfDim==1) then
        !#########################################################################
        !#######                                                           #######
        !#######                +-----------+-------------+                #######
        !#######            (1)             (2)            (3)             #######
        !#######                                                           #######
        !#########################################################################
        !allocate(obj%Nmat(obj%NumOfNode,obj%NumOfDim) )
        !
        !obj%Nmat(1,1)=0.50d0*( 1.0d0-gzi(1))
        !obj%Nmat(1,2)=0.50d0*(-1.0d0+gzi(1))
        !obj%Nmat(1,3)=0.50d0*( 1.0d0-gzi(1))
        !
        !obj%ErrorMsg="Succeed::GetShapeFunction "
        !obj%ierr=0
        print *, "ERROR::GetShapeFunction"
        obj%ErrorMsg="ERROR::GetShapeFunction "
        obj%ierr=1
    elseif(obj%NumOfDim==2) then
        !#########################################################################
        !#######                                                           #######
        !#######          (1)   +-------------------------+                #######
        !#######                 \                       /  (3)            #######
        !#######                   \                   /                   #######
        !#######                     \               /                     #######
        !#######                       \           /                       #######
        !#######                         \       /                         #######
        !#######                           \   /                           #######        
        !#######                       (2)   +                             #######        
        !#########################################################################
        
        
        print *, "ERROR::GetShapeFunction"
        obj%ErrorMsg="ERROR::GetShapeFunction "
        obj%ierr=1
    else

        print *, "ERROR::GetShapeFunction"
        obj%ErrorMsg="ERROR::GetShapeFunction "
        obj%ierr=1
    endif
elseif(obj%NumOfNode==4) then
    if(obj%NumOfDim==1) then
        !#########################################################################
        !#######                                                           #######
        !#######                +-------+---------+-------+                #######
        !#######          (1)          (2)        (3)       (4)            #######
        !#######                                                           #######
        !#########################################################################
        !obj%ErrorMsg="Succeed::GetShapeFunction "
        !obj%ierr=0
        print *, "ERROR::GetShapeFunction"
        obj%ErrorMsg="ERROR::GetShapeFunction "
        obj%ierr=1
    elseif(obj%NumOfDim==2) then
        !#########################################################################
        !#######                                                           #######
        !#######           (1)  +-------------------------+  (4)           #######
        !#######                |                         |                #######
        !#######                |                         |                #######
        !#######                |                         |                #######
        !#######                |                         |                #######
        !#######                |                         |                #######
        !#######                |                         |                #######        
        !#######           (2)  +-------------------------+  (3)           #######        
        !#########################################################################
        
		obj%Nmat(1)=1.0d0/4.0d0*(1.0d0-obj%gzi(1))*(1.0d0-obj%gzi(2))
	    obj%Nmat(2)=1.0d0/4.0d0*(1.0d0+obj%gzi(1))*(1.0d0-obj%gzi(2))
		obj%Nmat(3)=1.0d0/4.0d0*(1.0d0+obj%gzi(1))*(1.0d0+obj%gzi(2))
		obj%Nmat(4)=1.0d0/4.0d0*(1.0d0-obj%gzi(1))*(1.0d0+obj%gzi(2))
	  
        obj%ErrorMsg="Succeed::GetShapeFunction "
        obj%ierr=0
    elseif(obj%NumOfDim==3) then
        !#########################################################################
        !#######                    (4)   +                                #######
        !#######                        /   \                              #######
        !#######                      /       \                            #######
        !#######                    /           \                          #######
        !#######                  /           ___+    (3)                  #######
        !#######                /  ___----'''     \                        #######
        !#######          (1)  +  -                 \                      #######
        !#######                '''-->---____       \                      #######        
        !#######                              -->--- +   (2)               #######        
        !#########################################################################
        
        print *, "ERROR::GetShapeFunction"
        obj%ErrorMsg="ERROR::GetShapeFunction "
        obj%ierr=1
    else
        print *, "ERROR::GetShapeFunction"
        obj%ErrorMsg="ERROR::GetShapeFunction "
        obj%ierr=1
    endif
    
    
elseif(obj%NumOfNode==5) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==6) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==7) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==8) then
    if(obj%NumOfDim==3) then
        !#########################################################################
        !#######           (8)   +-------------------------+ (7)           #######
        !#######                /|                        /|               #######
        !#######               / |                  (6)  / |               #######
        !#######          (5) +--|----------------------+  |               #######
        !#######              |  |                      |  |               #######
        !#######              |  +----------------------|--+ (3)           #######
        !#######              | / (4)                   | /                #######
        !#######              |/                        |/                 #######        
        !#######          (1) +-------------------------+ (2)              #######        
        !#########################################################################
        
        obj%Nmat(1)=1.0d0/8.0d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 - obj%gzi(3))
	    obj%Nmat(2)=1.0d0/8.0d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 - obj%gzi(3))
		obj%Nmat(3)=1.0d0/8.0d0*(1.0d0 + obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 - obj%gzi(3))
		obj%Nmat(4)=1.0d0/8.0d0*(1.0d0 - obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 - obj%gzi(3))
        obj%Nmat(5)=1.0d0/8.0d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 + obj%gzi(3))
	    obj%Nmat(6)=1.0d0/8.0d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 + obj%gzi(3))
		obj%Nmat(7)=1.0d0/8.0d0*(1.0d0 + obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 + obj%gzi(3))
		obj%Nmat(8)=1.0d0/8.0d0*(1.0d0 - obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 + obj%gzi(3))
	  

        obj%ErrorMsg="Succeed::GetShapeFunction "
        obj%ierr=0
    else
        print *, "ERROR::GetShapeFunction"
        obj%ErrorMsg="ERROR::GetShapeFunction "
        obj%ierr=1
    endif
    
elseif(obj%NumOfNode==9) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==10) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==11) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==12) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==13) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==14) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==15) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==16) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==17) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==18) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==19) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==20) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==21) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==22) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==23) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==24) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
else
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
endif