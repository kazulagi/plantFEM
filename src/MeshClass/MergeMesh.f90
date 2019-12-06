!MergeObjects if the array is allocated.

    ! ========= Merge nodes  ============
    num1=size(inobj1%NodCoord,1)
    node_num1=num1
    num2=size(inobj2%NodCoord,1)
    num3=size(inobj2%NodCoord,2)
    if(num3 /= size(inobj1%NodCoord,1) )then
        outobj%ErrorMsg="MergeMesh >> num3 /= inobj1%NodCoord,1"
    endif

    allocate(outobj%NodCoord(num1+num2, num3))
    do i=1,num1
        outobj%NodCoord(i,:)=inobj1%NodCoord(i,:)
    enddo
    do i=1,num2
        outobj%NodCoord(i+num1,:)=inobj2%NodCoord(i,:)
    enddo

    ! update subdomain infomation
    if(allocated(inobj1%SubMeshNodFromTo) )then
        if(allocated(inobj2%SubMeshNodFromTo) )then
            if(allocated(outobj%SubMeshNodFromTo) )then
                deallocate(outobj%SubMeshNodFromTo)
            endif
            allocate(outobj%SubMeshNodFromTo(2,3) )
            outobj%SubMeshNodFromTo(1,1)=1 !subdomain ID
            outobj%SubMeshNodFromTo(1,2)=1
            outobj%SubMeshNodFromTo(1,3)=num1
            
            outobj%SubMeshNodFromTo(2,1)=2 !subdomain ID
            outobj%SubMeshNodFromTo(2,2)=num1+1 !node id starts from
            outobj%SubMeshNodFromTo(2,3)=num1+num2 !node id goes to
        else
            if(allocated(outobj%SubMeshNodFromTo) )then
                deallocate(outobj%SubMeshNodFromTo)
            endif
            allocate(outobj%SubMeshNodFromTo(2,3) )
            outobj%SubMeshNodFromTo(1,1)=1 !subdomain ID
            outobj%SubMeshNodFromTo(1,2)=1
            outobj%SubMeshNodFromTo(1,3)=num1
            
            outobj%SubMeshNodFromTo(2,1)=2 !subdomain ID
            outobj%SubMeshNodFromTo(2,2)=num1+1 !node id starts from
            outobj%SubMeshNodFromTo(2,3)=num1+num2 !node id goes to
        endif    
    else
        if(allocated(inobj2%SubMeshNodFromTo) )then
            if(allocated(outobj%SubMeshNodFromTo) )then
                deallocate(outobj%SubMeshNodFromTo)
            endif
            allocate(outobj%SubMeshNodFromTo(2,3) )
            outobj%SubMeshNodFromTo(1,1)=1 !subdomain ID
            outobj%SubMeshNodFromTo(1,2)=1
            outobj%SubMeshNodFromTo(1,3)=num1
            
            outobj%SubMeshNodFromTo(2,1)=2 !subdomain ID
            outobj%SubMeshNodFromTo(2,2)=num1+1 !node id starts from
            outobj%SubMeshNodFromTo(2,3)=num1+num2 !node id goes to
        else
            if(allocated(outobj%SubMeshNodFromTo) )then
                deallocate(outobj%SubMeshNodFromTo)
            endif
            allocate(outobj%SubMeshNodFromTo(2,3) )
            outobj%SubMeshNodFromTo(1,1)=1 !subdomain ID
            outobj%SubMeshNodFromTo(1,2)=1
            outobj%SubMeshNodFromTo(1,3)=num1
            
            outobj%SubMeshNodFromTo(2,1)=2 !subdomain ID
            outobj%SubMeshNodFromTo(2,2)=num1+1 !node id starts from
            outobj%SubMeshNodFromTo(2,3)=num1+num2 !node id goes to
        endif    
    endif
    ! ========= Merge nodes  ============
    


    ! ========= Merge elements  ============
    num1=size(inobj1%ElemNod,1)
    num2=size(inobj2%ElemNod,1)
    num3=size(inobj2%ElemNod,2)
    if(num3 /= size(inobj1%ElemNod,1) )then
        outobj%ErrorMsg="MergeMesh >> num3 /= inobj1%ElemNod,1"
    endif

    allocate(outobj%ElemNod(num1+num2, num3))
    do i=1,num1
        outobj%ElemNod(i,:)=inobj1%ElemNod(i,:)
    enddo
    do i=1,num2
        outobj%ElemNod(i+num1,:)=inobj2%ElemNod(i,:)+node_num1
    enddo
    ! update subdomain infomation
    if(allocated(inobj1%SubMeshElemFromTo) )then
        if(allocated(inobj2%SubMeshElemFromTo) )then
            if(allocated(outobj%SubMeshElemFromTo) )then
                deallocate(outobj%SubMeshElemFromTo)
            endif
            allocate(outobj%SubMeshElemFromTo(2,3) )
            outobj%SubMeshElemFromTo(1,1)=1 !subdomain ID
            outobj%SubMeshElemFromTo(1,2)=1
            outobj%SubMeshElemFromTo(1,3)=num1
            
            outobj%SubMeshElemFromTo(2,1)=2 !subdomain ID
            outobj%SubMeshElemFromTo(2,2)=num1+1 !node id starts from
            outobj%SubMeshElemFromTo(2,3)=num1+num2 !node id goes to
        else
            if(allocated(outobj%SubMeshElemFromTo) )then
                deallocate(outobj%SubMeshElemFromTo)
            endif
            allocate(outobj%SubMeshElemFromTo(2,3) )
            outobj%SubMeshElemFromTo(1,1)=1 !subdomain ID
            outobj%SubMeshElemFromTo(1,2)=1
            outobj%SubMeshElemFromTo(1,3)=num1
            
            outobj%SubMeshElemFromTo(2,1)=2 !subdomain ID
            outobj%SubMeshElemFromTo(2,2)=num1+1 !node id starts from
            outobj%SubMeshElemFromTo(2,3)=num1+num2 !node id goes to
        endif    
    else
        if(allocated(inobj2%SubMeshElemFromTo) )then
            if(allocated(outobj%SubMeshElemFromTo) )then
                deallocate(outobj%SubMeshElemFromTo)
            endif
            allocate(outobj%SubMeshElemFromTo(2,3) )
            outobj%SubMeshElemFromTo(1,1)=1 !subdomain ID
            outobj%SubMeshElemFromTo(1,2)=1
            outobj%SubMeshElemFromTo(1,3)=num1
            
            outobj%SubMeshElemFromTo(2,1)=2 !subdomain ID
            outobj%SubMeshElemFromTo(2,2)=num1+1 !node id starts from
            outobj%SubMeshElemFromTo(2,3)=num1+num2 !node id goes to
        else
            if(allocated(outobj%SubMeshElemFromTo) )then
                deallocate(outobj%SubMeshElemFromTo)
            endif
            allocate(outobj%SubMeshElemFromTo(2,3) )
            outobj%SubMeshElemFromTo(1,1)=1 !subdomain ID
            outobj%SubMeshElemFromTo(1,2)=1
            outobj%SubMeshElemFromTo(1,3)=num1
            
            outobj%SubMeshElemFromTo(2,1)=2 !subdomain ID
            outobj%SubMeshElemFromTo(2,2)=num1+1 !node id starts from
            outobj%SubMeshElemFromTo(2,3)=num1+num2 !node id goes to
        endif    
    endif
    ! ========= Merge elements  ============
    

    ! ========= Merge Facet Elements  ============
    num1=size(inobj1%FacetElemNod,1)
    num2=size(inobj2%FacetElemNod,1)
    num3=size(inobj2%FacetElemNod,2)
    if(num3 /= size(inobj1%FacetElemNod,1) )then
        outobj%ErrorMsg="MergeMesh >> num3 /= inobj1%ElemNod,1"
    endif

    allocate(outobj%FacetElemNod(num1+num2, num3))
    do i=1,num1
        outobj%FacetElemNod(i,:)=inobj1%FacetElemNod(i,:)
    enddo
    do i=1,num2
        outobj%FacetElemNod(i+num1,:)=inobj2%FacetElemNod(i,:)+node_num1
    enddo

    
    ! ========= Merge Facet Elements  ============


    ! ========= Merge surface elements  ============
    num1=size(inobj1%SurfaceLine2D,1)
    num2=size(inobj2%SurfaceLine2D,1)
    
    allocate(outobj%SurfaceLine2D(num1+num2))
    do i=1,num1
        outobj%SurfaceLine2D(i)=inobj1%SurfaceLine2D(i)
    enddo
    do i=1,num2
        outobj%SurfaceLine2D(i+num1)=inobj2%SurfaceLine2D(i)+node_num1
    enddo

    ! update subdomain infomation
    if(allocated(inobj1%SubMeshSurfFromTo) )then
        if(allocated(inobj2%SubMeshSurfFromTo) )then
            if(allocated(outobj%SubMeshSurfFromTo) )then
                deallocate(outobj%SubMeshSurfFromTo)
            endif
            allocate(outobj%SubMeshSurfFromTo(2,3) )
            outobj%SubMeshSurfFromTo(1,1)=1 !subdomain ID
            outobj%SubMeshSurfFromTo(1,2)=1
            outobj%SubMeshSurfFromTo(1,3)=num1
            
            outobj%SubMeshSurfFromTo(2,1)=2 !subdomain ID
            outobj%SubMeshSurfFromTo(2,2)=num1+1 !node id starts from
            outobj%SubMeshSurfFromTo(2,3)=num1+num2 !node id goes to
        else
            if(allocated(outobj%SubMeshSurfFromTo) )then
                deallocate(outobj%SubMeshSurfFromTo)
            endif
            allocate(outobj%SubMeshSurfFromTo(2,3) )
            outobj%SubMeshSurfFromTo(1,1)=1 !subdomain ID
            outobj%SubMeshSurfFromTo(1,2)=1
            outobj%SubMeshSurfFromTo(1,3)=num1
            
            outobj%SubMeshSurfFromTo(2,1)=2 !subdomain ID
            outobj%SubMeshSurfFromTo(2,2)=num1+1 !node id starts from
            outobj%SubMeshSurfFromTo(2,3)=num1+num2 !node id goes to
        endif    
    else
        if(allocated(inobj2%SubMeshSurfFromTo) )then
            if(allocated(outobj%SubMeshSurfFromTo) )then
                deallocate(outobj%SubMeshSurfFromTo)
            endif
            allocate(outobj%SubMeshSurfFromTo(2,3) )
            outobj%SubMeshSurfFromTo(1,1)=1 !subdomain ID
            outobj%SubMeshSurfFromTo(1,2)=1
            outobj%SubMeshSurfFromTo(1,3)=num1
            
            outobj%SubMeshSurfFromTo(2,1)=2 !subdomain ID
            outobj%SubMeshSurfFromTo(2,2)=num1+1 !node id starts from
            outobj%SubMeshSurfFromTo(2,3)=num1+num2 !node id goes to
        else
            if(allocated(outobj%SubMeshSurfFromTo) )then
                deallocate(outobj%SubMeshSurfFromTo)
            endif
            allocate(outobj%SubMeshSurfFromTo(2,3) )
            outobj%SubMeshSurfFromTo(1,1)=1 !subdomain ID
            outobj%SubMeshSurfFromTo(1,2)=1
            outobj%SubMeshSurfFromTo(1,3)=num1
            
            outobj%SubMeshSurfFromTo(2,1)=2 !subdomain ID
            outobj%SubMeshSurfFromTo(2,2)=num1+1 !node id starts from
            outobj%SubMeshSurfFromTo(2,3)=num1+num2 !node id goes to
        endif    
    endif
    ! ========= Merge surface elements  ============


    ! ========= Merge Material ID ==================
    num1=size(inobj1%ElemMat,1)
    num2=size(inobj2%ElemMat,1)
    if(num3 /= size(inobj1%ElemMat,1) )then
        outobj%ErrorMsg="MergeMesh >> num3 /= inobj1%ElemMat,1"
    endif

    allocate(outobj%ElemMat(num1+num2))
    do i=1,num1
        outobj%ElemMat(i)=inobj1%ElemMat(i)
    enddo
    do i=1,num2
        outobj%ElemMat(i+num1)=inobj2%ElemMat(i)+Maxval(inobj1%ElemMat)
    enddo
    ! ========= Merge Material ID ==================