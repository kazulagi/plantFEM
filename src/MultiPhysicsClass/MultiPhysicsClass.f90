module MultiPhysicsClass
    use, intrinsic :: iso_fortran_env
    use FEMIfaceClass
    use FEMDomainClass

    type :: MultiPhysics_
        type(FEMIface_),pointer :: FEMIface
        type(FEMDomain_),pointer :: FEMDomain1
        type(FEMDomain_),pointer :: FEMDomain2
    contains
        procedure :: SyncMesh => SyncMesh
    end type

contains




! #############################################################
subroutine SyncMesh(obj,Arrow)
    class(MultiPhysics_),intent(inout)::obj
    character*2,optional,intent(in)::Arrow

    integer(int32) :: i,j,n,m,dim_num1,dim_num2,elem_num1,elem_num2,node_num1,node_num2,ierr

    
    node_num1=size(obj%FEMDomain1%Mesh%NodCoord,1)
    node_num2=size(obj%FEMDomain2%Mesh%NodCoord,1)
    elem_num1=size(obj%FEMDomain1%Mesh%ElemNod,1)
    elem_num2=size(obj%FEMDomain2%Mesh%ElemNod,1)
    dim_num1=size(obj%FEMDomain1%Mesh%NodCoord,2)
    dim_num2=size(obj%FEMDomain2%Mesh%NodCoord,2)


    n=abs(node_num1-node_num2)+abs(elem_num1-elem_num2)+abs(dim_num1-dim_num2)

    if(n/=0)then
        stop "ERROR :: MultiPhysicsIfaceClass >> SyncMesh >> size of in/out mesh are different!"
        return
    else
        if(present(Arrow) )then
            if(Arrow=="->" .or. Arrow=="=>")then
                obj%FEMDomain2%Mesh%NodCoord(:,:)=obj%FEMDomain1%Mesh%NodCoord(:,:)  
            elseif(Arrow=="<-" .or. Arrow=="<=")then
                obj%FEMDomain1%Mesh%NodCoord(:,:)=obj%FEMDomain2%Mesh%NodCoord(:,:)    
            else
                stop "MultiPhysicsIfaceClass>>SyncMesh>> Arrow should be -> or <-"
            endif
        else
            obj%FEMDomain1%Mesh%NodCoord(:,:)=obj%FEMDomain2%Mesh%NodCoord(:,:)
        endif
    endif
end subroutine 

end module 