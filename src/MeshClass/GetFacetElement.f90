
    if(allocated(obj%FacetElemNod) )then
        deallocate(obj%FacetElemNod)
    endif
    NumOfElem = size(obj%ElemNod,1) 
    NumOfDim  = size(obj%NodCoord,2)
    NumNodePerElem = size(obj%ElemNod,2)

    If(NumOfDim < 2 .or. NumOfDim > 4 ) then
        obj%ErrorMsg = "ERROR::GetFaceElement.f90 >> NumOfDim = 2 or 3"
        return
    endif

    if(NumOfDim==2)then
        ! initialization only for linear_triangle&rectangular ::
        if(allocated(obj%FacetElemNod) ) then
            deallocate(obj%FacetElemNod)
        endif
        allocate(obj%FacetElemNod(NumOfElem*NumNodePerElem,2) )
        obj%FacetElemNod(:,:) = 0


        ! trial mode
        do i=1,NumOfElem
            do j=1,NumNodePerElem
                id_1=mod(j+NumNodePerElem   ,NumNodePerElem)
                id_2=mod(j+NumNodePerElem+1 ,NumNodePerElem)

                if(id_1==0)then
                    id_1=NumNodePerElem
                endif
                if(id_2==0)then
                    id_2=NumNodePerElem
                endif

                obj%FacetElemNod( NumNodePerElem*(i-1)+j,1) = obj%ElemNod(i,id_1)
                obj%FacetElemNod( NumNodePerElem*(i-1)+j,2) = obj%ElemNod(i,id_2)

            enddo
        enddo

        ! cut off overlapped facets
        do i=1,size(obj%FacetElemNod,1)-1
            if(obj%FacetElemNod(i,1)==-1 )then
                cycle
            endif
            do j=i+1,size(obj%FacetElemNod,1)
                if(obj%FacetElemNod(i,1) == obj%FacetElemNod(j,2) .and. &
                    obj%FacetElemNod(i,2) == obj%FacetElemNod(j,1) )then

                    obj%FacetElemNod(i,:)=-1
                    obj%FacetElemNod(j,:)=-1

                    exit
                endif

                if(obj%FacetElemNod(i,1)==-1 )then
                    exit
                endif
            enddo

        enddo


        allocate(buffer(size(obj%FacetElemNod,1),size(obj%FacetElemNod,2) ))

        buffer(:,:)=0
        j=0
        k=0
        do i=1,size(obj%FacetElemNod,1)
            if(obj%FacetElemNod(i,1)==-1)then
                    cycle
            else
                k=k+1
                buffer(k,:)=obj%FacetElemNod(i,:)
            endif
        enddo

        deallocate(obj%FacetElemNod)
        allocate(obj%FacetElemNod(k,2) )

        do i=1,size(obj%FacetElemNod,1)
            obj%FacetElemNod(i,:)=buffer(i,:)
        enddo
    elseif(NumOfDim==3 )then

        ! initialization only for  Hexahedral/tetrahedron::
        if(allocated(obj%FacetElemNod) ) then
            deallocate(obj%FacetElemNod)
        endif

        NumOfElem=size(obj%ElemNod,1)
        if(NumNodePerElem==4)then
            allocate(obj%FacetElemNod(NumOfElem*4,3),id(3),idr(3) )
        elseif(NumNodePerElem==8)then
            allocate(obj%FacetElemNod(NumOfElem*6,4),id(4),idr(4) )
        else
            stop "ERROR :: GetFacetElement :: only for  Hexahedral/tetrahedron #"
        endif
        obj%FacetElemNod(:,:) = 0



        ! trial mode
        do i=1,size(obj%ElemNod,1)
            if(NumNodePerElem==4)then
                obj%FacetElemNod(  (i-1)*4+1 ,1) = obj%ElemNod(i,1)
                obj%FacetElemNod(  (i-1)*4+1 ,2) = obj%ElemNod(i,2)
                obj%FacetElemNod(  (i-1)*4+1 ,3) = obj%ElemNod(i,3)

                obj%FacetElemNod(  (i-1)*4+2 ,1) = obj%ElemNod(i,1)
                obj%FacetElemNod(  (i-1)*4+2 ,2) = obj%ElemNod(i,2)
                obj%FacetElemNod(  (i-1)*4+2 ,3) = obj%ElemNod(i,4)

                obj%FacetElemNod(  (i-1)*4+3 ,1) = obj%ElemNod(i,2)
                obj%FacetElemNod(  (i-1)*4+3 ,2) = obj%ElemNod(i,3)
                obj%FacetElemNod(  (i-1)*4+3 ,3) = obj%ElemNod(i,4)

                obj%FacetElemNod(  (i-1)*4+4 ,1) = obj%ElemNod(i,3)
                obj%FacetElemNod(  (i-1)*4+4 ,2) = obj%ElemNod(i,1)
                obj%FacetElemNod(  (i-1)*4+4 ,3) = obj%ElemNod(i,4)

            elseif(NumNodePerElem==8)then
                obj%FacetElemNod(  (i-1)*6+1 ,1) = obj%ElemNod(i,4)
                obj%FacetElemNod(  (i-1)*6+1 ,2) = obj%ElemNod(i,3)
                obj%FacetElemNod(  (i-1)*6+1 ,3) = obj%ElemNod(i,2)
                obj%FacetElemNod(  (i-1)*6+1 ,4) = obj%ElemNod(i,1)

                obj%FacetElemNod(  (i-1)*6+2 ,1) = obj%ElemNod(i,1)
                obj%FacetElemNod(  (i-1)*6+2 ,2) = obj%ElemNod(i,2)
                obj%FacetElemNod(  (i-1)*6+2 ,3) = obj%ElemNod(i,6)
                obj%FacetElemNod(  (i-1)*6+2 ,4) = obj%ElemNod(i,5)

                obj%FacetElemNod(  (i-1)*6+3 ,1) = obj%ElemNod(i,2)
                obj%FacetElemNod(  (i-1)*6+3 ,2) = obj%ElemNod(i,3)
                obj%FacetElemNod(  (i-1)*6+3 ,3) = obj%ElemNod(i,7)
                obj%FacetElemNod(  (i-1)*6+3 ,4) = obj%ElemNod(i,6)

                obj%FacetElemNod(  (i-1)*6+4 ,1) = obj%ElemNod(i,3)
                obj%FacetElemNod(  (i-1)*6+4 ,2) = obj%ElemNod(i,4)
                obj%FacetElemNod(  (i-1)*6+4 ,3) = obj%ElemNod(i,8)
                obj%FacetElemNod(  (i-1)*6+4 ,4) = obj%ElemNod(i,7)

                obj%FacetElemNod(  (i-1)*6+5 ,1) = obj%ElemNod(i,4)
                obj%FacetElemNod(  (i-1)*6+5 ,2) = obj%ElemNod(i,1)
                obj%FacetElemNod(  (i-1)*6+5 ,3) = obj%ElemNod(i,5)
                obj%FacetElemNod(  (i-1)*6+5 ,4) = obj%ElemNod(i,8)

                obj%FacetElemNod(  (i-1)*6+6 ,1) = obj%ElemNod(i,5)
                obj%FacetElemNod(  (i-1)*6+6 ,2) = obj%ElemNod(i,6)
                obj%FacetElemNod(  (i-1)*6+6 ,3) = obj%ElemNod(i,7)
                obj%FacetElemNod(  (i-1)*6+6 ,4) = obj%ElemNod(i,8)

            else
                stop "ERROR :: GetFacetElement :: only for  Hexahedral/tetrahedron ##"
            endif
        enddo


        ! cut off overlapped facets
        do i=1,size(obj%FacetElemNod,1)-1
            if(obj%FacetElemNod(i,1)==-1 )then
                cycle
            endif
            do j=i+1,size(obj%FacetElemNod,1)

                if(size(obj%FacetElemNod,2)==3 .or. size(obj%FacetElemNod,2)==4 )then
                    id(:)=obj%FacetElemNod(i,:)
                    idr(:)=obj%FacetElemNod(j,:)
                    call heapsort(size(id) ,id)
                    call heapsort(size(idr) ,idr)
                    id_1=dot_product(id-idr,id-idr)

                    if(id_1==0)then
                        obj%FacetElemNod(i,:)=-1
                        obj%FacetElemNod(j,:)=-1
                    endif
                else
                    stop "ERROR :: GetFacetElement :: only for  Hexahedral/tetrahedron ##"
                endif


            enddo
        enddo

        allocate(buffer(size(obj%FacetElemNod,1),size(obj%FacetElemNod,2) ))

        buffer(:,:)=0
        j=0
        k=0
        do i=1,size(obj%FacetElemNod,1)
            if(obj%FacetElemNod(i,1)==-1)then
                cycle
            else
                k=k+1
                buffer(k,:)=obj%FacetElemNod(i,:)
            endif
        enddo


        deallocate(obj%FacetElemNod)
        allocate(obj%FacetElemNod(k, size(buffer,2) ) )

        do i=1,size(obj%FacetElemNod,1)
            obj%FacetElemNod(i,:)=buffer(i,:)
        enddo

    endif