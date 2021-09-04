use SoybeanClass

implicit none

type(Soybean_) :: soy
character(200) :: line

do
    print *, ">>>"
    read(*,*) line
    if(index(line,"exit")/=0) then
        print *, "bye"
        exit
    endif

    if(index(line,"init")/=0) then
        print *, "configuration file :: (default=Tutorial/obj/realSoybeanConfig.json)"
        read(*,*) line
        if(index(line,"json")==0 )then
            line = "Tutorial/obj/realSoybeanConfig.json"
            print *, "Default mode is selected."
        endif
        print *, "creating a soybean object..."
        call soy%init(config=trim(line)) 
        print *, "[ok] Done"
        cycle
    endif

    if(index(line,"save")/=0)then
        print *, "File format (vtk, stl, json, msh)"
        read(*,*)line
        
        if(index(line,"vtk")/=0)then
            print *, "File Name"
            read(*,*) line  
            call soy%vtk(line)
            cycle
        elseif(index(line,"stl")/=0)then
            print *, "File Name"
            read(*,*) line  
            call soy%stl(line)
            cycle
        elseif(index(line,"msh")/=0)then
            print *, "File Name"
            read(*,*) line  
            call soy%msh(line)
            cycle
        elseif(index(line,"json")/=0)then
            print *, "File Name"
            read(*,*) line  
            call soy%json(line)
            cycle
        else
            print *, "No format"
            cycle
        endif
    endif

    if(index(line,"numleaf") /=0)then
        print *, soy%numleaf()
    endif

    if(index(line,"numstem")  /=0)then
        print *, soy%numstem()
    endif


    if(index(line,"numroot") /=0 )then
        print *, soy%numroot()
    endif


    if(index(line,"remove") /=0 )then
        call soy%remove()
        print *, "removed a soybean"
        cycle
    endif

enddo

end