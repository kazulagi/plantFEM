module AnalystClass
    use uuid_module
    use IOClass
    use ListClass
    use MathClass
    use ArrayClass
    implicit none
    
    type :: PDF_
        character(:),allocatable :: scene_name       
        logical :: confidential = .false. 
    end type


    type :: Plot_
        real(real64),allocatable :: x_list(:)
        real(real64),allocatable :: y_list(:,:)
        character(:),allocatable :: x_label
        character(:),allocatable :: y_label
        character(:),allocatable :: title 
        logical :: with_line = .false.
        logical :: logscale = .false.  
        logical :: x_logscale = .false.  
        logical :: y_logscale = .false.  
        logical :: z_logscale = .false.  
        type(List_) :: regend
    end type


    type :: Analyst_

    contains
        procedure,public :: to_pdf => to_pdf_from_plots_analystclass
        procedure,public :: render => render_analystclass


        procedure,pass :: to_plot_analystclass_vec_vec
        procedure,pass :: to_plot_analystclass_vec_array
        generic :: to_plot => to_plot_analystclass_vec_vec,&
            to_plot_analystclass_vec_array

        procedure,pass :: pdf_from_plots_analystclass
        procedure,pass :: pdf_analystclass_vec_vec
        procedure,pass :: pdf_analystclass_vec_array
        generic :: pdf => pdf_analystclass_vec_vec,&
            pdf_analystclass_vec_array,&
            pdf_from_plots_analystclass
    end type

contains

! ###################################################
function to_plot_analystclass_vec_vec(this,x_list,y_list,x_label,y_label,&
    title,with_line,logscale,x_logscale,y_logscale,z_logscale,regend) result(ret)
    class(Analyst_),intent(in) :: this
    type(Plot_) :: ret
    real(real64),intent(in) :: x_list(:),y_list(:)
    character(*),intent(in) :: x_label,y_label,title
    type(List_),optional,intent(in) :: regend
    logical,intent(in) :: with_line

    logical,optional,intent(in) :: logscale,x_logscale,y_logscale,z_logscale

    ret % x_list    = x_list

    ret % y_list    = zeros(size(y_list),1 )
    ret % y_list(:,1)    = y_list(:)
    
    ret % x_label   = x_label
    ret % y_label   = y_label
    ret % title     = title
    ret % with_line = with_line
    if(present(regend) )then
        ret % regend = regend
    endif

    if(present(logscale) )then
        ret % logscale  = logscale
    endif

    if(present(x_logscale) )then
        ret % x_logscale  = x_logscale
    endif

    if(present(y_logscale) )then
        ret % y_logscale  = y_logscale
    endif

    if(present(z_logscale) )then
        ret % z_logscale  = z_logscale
    endif

end function
! ###################################################


! ###################################################
function to_plot_analystclass_vec_array(this,x_list,y_list,x_label,y_label,&
    title,with_line,logscale,x_logscale,y_logscale,z_logscale,regend) result(ret)
    class(Analyst_),intent(in) :: this
    type(Plot_) :: ret
    real(real64),intent(in) :: x_list(:),y_list(:,:)
    character(*),intent(in) :: x_label,y_label,title
    type(List_),intent(in) :: regend
    logical,intent(in) :: with_line


    logical,optional,intent(in) :: logscale,x_logscale,y_logscale,z_logscale

    ret % x_list    = x_list

    ret % y_list    = y_list
    
    ret % x_label   = x_label
    ret % y_label   = y_label
    ret % title     = title
    ret % with_line = with_line
    ret % regend = regend
    
    if(present(logscale) )then
        ret % logscale  = logscale
    endif

    if(present(x_logscale) )then
        ret % x_logscale  = x_logscale
    endif

    if(present(y_logscale) )then
        ret % y_logscale  = y_logscale
    endif

    if(present(z_logscale) )then
        ret % z_logscale  = z_logscale
    endif

    
end function
! ###################################################


! ###################################################
subroutine pdf_analystclass_vec_vec(this,name,x_list,y_list,x_label,y_label,&
    title,with_line,logscale,x_logscale,y_logscale,z_logscale,regend)
    class(Analyst_),intent(in) :: this
    real(real64),intent(in) :: x_list(:),y_list(:)
    character(*),intent(in) :: name,x_label,y_label,title
    logical,intent(in) :: with_line
    type(List_),optional,intent(in) :: regend
    logical,optional,intent(in) :: logscale,x_logscale,y_logscale,z_logscale

    character(:),allocatable :: wl
    type(IO_) :: f
    integer(int32) :: i

    if(with_line)then
        wl = "w l "
    else
        wl = " "
    endif
    
    call f%open(name+".csv","w")
    do i = 1,min(size(x_list),size(y_list) )
        write(f%fh,*) x_list(i),",",y_list(i)
    enddo
    call f%close()


    call f%open(name+".gp","w")
    
    
    call f%write("set terminal pdf")
    call f%write("set output '"+name+"'")
    call f%write("set datafile separator ','")
    call f%write("set grid")
    call f%write('set key font "Times, 14"')

    if(.not.present(regend) )then
        call f%write("unset key")
    endif
    call f%write("set xtics font 'Times, 14'")
    call f%write("set ytics font 'Times, 14'")
    if(present(logscale) )then
        if(logscale)then
            call f%write("set logscale")
        endif
    endif
    if(present(x_logscale) )then
        if(x_logscale)then
            call f%write("set logscale x")
        endif
    endif

    if(present(y_logscale) )then
        if(y_logscale)then
            call f%write("set logscale y")
        endif
    endif

    if(present(z_logscale) )then
        if(z_logscale)then
            call f%write("set logscale z")
        endif
    endif

    call f%write("set xlabel '"+x_label+"'  font 'Times, 14'")
    call f%write("set ylabel '"+y_label+"'  font 'Times, 14'")
    call f%write("set title '"+title+"' font 'Times, 14'")

    if(present(regend) )then
        call f%write("plot '"+name+".csv' u 1:2 "+wl+" title '"+regend%get(1)+"'" )
    else
        call f%write("plot '"+name+".csv' u 1:2 "+wl)
    endif
    
    
    call f%write("exit")
    call f%close()

    call system("gnuplot "+name+".gp")
    

end subroutine
! ###################################################

! ###################################################
subroutine pdf_analystclass_vec_array(this,name,x_list,y_list,x_label,y_label,&
    title,with_line,logscale,x_logscale,y_logscale,z_logscale,regend)
    class(Analyst_),intent(in) :: this
    real(real64),intent(in) :: x_list(:),y_list(:,:)
    character(*),intent(in) :: name,x_label,y_label,title
    logical,intent(in) :: with_line
    type(List_),intent(in) :: regend
    logical,optional,intent(in) :: logscale,x_logscale,y_logscale,z_logscale
    character(:),allocatable :: command
    character(:),allocatable :: wl
    type(IO_) :: f
    integer(int32) :: i

    if(with_line)then
        wl = "w l "
    else
        wl = " "
    endif
    
    call f%open(name+".tsv","w")
    do i = 1,min(size(x_list),size(y_list,1) )
        write(f%fh,*) x_list(i),y_list(i,:)
    enddo
    call f%close()


    call f%open(name+".gp","w")
    call f%write("set terminal pdf ")
    call f%write("set output '"+name+"'")
    
    call f%write("set grid")
    call f%write('set key font "Times, 14"')
    !call f%write("unset key")
    call f%write("set xtics font 'Times, 14'")
    call f%write("set ytics font 'Times, 14'")

    if(present(logscale) )then
        if(logscale)then
            call f%write("set logscale")
        endif
    endif
    if(present(x_logscale) )then
        if(x_logscale)then
            call f%write("set logscale x")
        endif
    endif

    if(present(y_logscale) )then
        if(y_logscale)then
            call f%write("set logscale y")
        endif
    endif

    if(present(z_logscale) )then
        if(z_logscale)then
            call f%write("set logscale z")
        endif
    endif

    

    call f%write("set xlabel '"+x_label+"'  font 'Times, 14' ")
    call f%write("set ylabel '"+y_label+"'  font 'Times, 14' ")
    call f%write("set title '"+title+"' font 'Times, 14'")
    call f%write("set multiplot")
    
    command = "plot '"+name+".tsv' u 1:2 "+wl+" title '"+regend%get(1)+"'"
    do i=2,size(y_list,2)
        command = command + " , " + " '"+name+".tsv' u 1:"+str(i+1)+" "+wl+" title '"+regend%get(i)+"'"
    enddo

    call f%write(command)
    
    call f%write("unset multiplot")
    
    call f%close()

    call system("gnuplot "+name+".gp")
    

end subroutine
! ###################################################

! ###################################################
subroutine pdf_from_plots_analystclass(this,name,plot)
    class(Analyst_),intent(in) :: this
    character(*),intent(in) :: name
    type(Plot_),intent(in) :: plot(:,:)
    character(:),allocatable :: command
    type(IO_) :: f, gp
    integer(int32) :: i,j,itr
    character(:),allocatable :: wl

    ! write data
    do i=1,size(plot,1)
        do j=1,size(plot,2)
            call f%open(name+zfill(i,4)+zfill(j,4)+".tsv","w")
            do itr = 1,min(size(plot(i,j)%x_list),size(plot(i,j)%y_list,1) )
                write(f%fh,*) plot(i,j)%x_list(itr),plot(i,j)%y_list(itr,:)
            enddo
            call f%close()
        enddo
    enddo

    

    call gp%open(name+".gp","w")
    call gp%write("set terminal pdf size 29.7cm,21.0cm")
    call gp%write("set output '"+name+"'")
    
    call gp%write("set grid")
    call gp%write('set key font "Times, 14"')
    !call gp%write("unset key")
    call gp%write("set xtics font 'Times, 14'")
    call gp%write("set ytics font 'Times, 14'")

    
    call gp%write("set multiplot layout "+str(size(plot,1) )+" , "+str(size(plot,2) ) )

    do i=1,size(plot,1)
        do j=1,size(plot,2)    

            if(plot(i,j)%logscale)then
                call gp%write("set logscale")
            endif
            if(plot(i,j)%x_logscale)then
                call gp%write("set logscale x")
            endif
            if(plot(i,j)%y_logscale)then
                call gp%write("set logscale y")
            endif
            if(plot(i,j)%z_logscale)then
                call gp%write("set logscale z")
            endif
        

        
            call gp%write("set xlabel '"+plot(i,j)%x_label+"'  font 'Times, 14'")
            call gp%write("set ylabel '"+plot(i,j)%y_label+"'  font 'Times, 14'")
            call gp%write("set title '"+plot(i,j)%title+"' font 'Times, 14'")

            if(plot(i,j)%with_line)then
                wl = "w l "
            else
                wl = " "
            endif

            command = "plot '"+name+zfill(i,4)+zfill(j,4)+".tsv' u 1:2 "+wl+" title '"+plot(i,j)%regend%get(1)+"'"
            do itr=2,size(plot(i,j)%y_list,2)
                command = command + " , " + " '"+name+zfill(i,4)+zfill(j,4)+".tsv' u 1:"+str(itr+1)+" "+wl &
                    +" title '"+plot(i,j)%regend%get(itr)+"'"
            enddo
        
            call gp%write(command)
        enddo
    enddo

    call gp%write("unset multiplot")
    call gp%close()
    call system("gnuplot "+name+".gp")
    


end subroutine
! ###################################################




! ###################################################
function to_pdf_from_plots_analystclass(this,plot,option) result(pdfobj)
    class(Analyst_),intent(in) :: this
    type(Plot_),intent(in) :: plot(:,:)
    character(*),optional,intent(in) :: option
    type(PDF_) :: pdfobj
    character(:),allocatable :: command
    type(IO_) :: f, gp
    integer(int32) :: i,j,itr
    character(:),allocatable :: wl
    character(:),allocatable :: name

    name = "analystclass_"+generate_uuid(1)

    ! write data
    do i=1,size(plot,1)
        do j=1,size(plot,2)
            call f%open(name+zfill(i,4)+zfill(j,4)+".tsv","w")
            do itr = 1,min(size(plot(i,j)%x_list),size(plot(i,j)%y_list,1) )
                write(f%fh,*) plot(i,j)%x_list(itr),plot(i,j)%y_list(itr,:)
            enddo
            call f%close()
        enddo
    enddo



    call gp%open(name+".gp","w")
    call gp%write("set terminal pdf size 29.7cm,21.0cm")
    call gp%write("set output '"+name+".pdf'")
    
    call gp%write("set grid")
    call gp%write('set key font "Times, 14"')
    !call gp%write("unset key")
    call gp%write("set xtics font 'Times, 14'")
    call gp%write("set ytics font 'Times, 14'")

    
    call gp%write("set multiplot layout "+str(size(plot,1) )+" , "+str(size(plot,2) ) )

    do i=1,size(plot,1)
        do j=1,size(plot,2)    

            if(plot(i,j)%logscale)then
                call gp%write("set logscale")
            endif
            if(plot(i,j)%x_logscale)then
                call gp%write("set logscale x")
            endif
            if(plot(i,j)%y_logscale)then
                call gp%write("set logscale y")
            endif
            if(plot(i,j)%z_logscale)then
                call gp%write("set logscale z")
            endif
        

        
            call gp%write("set xlabel '"+plot(i,j)%x_label+"'  font 'Times, 14'")
            call gp%write("set ylabel '"+plot(i,j)%y_label+"'  font 'Times, 14'")
            call gp%write("set title '"+plot(i,j)%title+"' font 'Times, 14'")

            if(plot(i,j)%with_line)then
                wl = "w l "
            else
                wl = " "
            endif

            command = "plot '"+name+zfill(i,4)+zfill(j,4)+".tsv' u 1:2 "+wl+" title '"+plot(i,j)%regend%get(1)+"'"
            do itr=2,size(plot(i,j)%y_list,2)
                command = command + " , " + " '"+name+zfill(i,4)+zfill(j,4)+".tsv' u 1:"+str(itr+1)+" "+wl &
                    +" title '"+plot(i,j)%regend%get(itr)+"'"
            enddo
        
            call gp%write(command)
        enddo
    enddo

    call gp%write("unset multiplot")
    call gp%close()

    if(present(option) )then
        if(index(option,"onfidential") /=0 .or. index(option,"ONFIDENTIAL")/=0 )then
            if(.not. f%exists("confidential.pdf") )then
                call f%download("https://www.plantfem.org/download/confidential.pdf")
            endif
            if( f%exists("confidential.pdf"))then
                pdfobj%confidential = .true.
            else
                print *, "[WARNING] to_pdf >> watermark 'confidential.pdf' is not &
                    downloaded. Please check internet connection."
            endif
        endif
    endif
    
    
    pdfobj%scene_name = name
    


end function
! ###################################################

subroutine render_analystclass(this,name,pdf)
    class(Analyst_),intent(in) :: this
    character(*),intent(in) :: name
    type(PDF_),intent(in) :: pdf(:)
    character(:),allocatable :: command
    integer(int32) :: i
    
    ! watermark
    do i=1,size(pdf)
        call system("gnuplot "+pdf(i)%scene_name+".gp")
        if(pdf(i)%confidential )then
            
            command = "pdftk "+pdf(i)%scene_name+".pdf " +" background confidential.pdf output "+pdf(i)%scene_name+"_c.pdf "
            call system(command)
            command = "mv "+pdf(i)%scene_name+"_c.pdf "+pdf(i)%scene_name+".pdf "
            call system(command)
        endif
    enddo

    command = "pdfunite "
    do i=1,size(pdf)
        
        command = command + pdf(i)%scene_name+".pdf "
    enddo
    command = command + " "+ name

    

    call system(command)


end subroutine


end module
