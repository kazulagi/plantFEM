module PlotClass
    use IOClass
    implicit none
    
    type :: SubPlot_
        character(:),allocatable :: xlabel
        character(:),allocatable :: ylabel
        character(:),allocatable :: name
        real(real64),allocatable :: xr(:)
        real(real64),allocatable :: yr(:)
        type(SubPlot_),pointer   :: subplot=> null()
        ! content type #1 plot
        real(real64),pointer :: x(:) => null()
        real(real64),pointer :: y(:) => null()
        ! content type #2 comment
        character(:),allocatable :: comment
    end type

    type :: Plot_ 
        type(SubPlot_),allocatable :: subplot(:,:)
    contains
        procedure,public :: setLayout => setLayoutPlotClass
        procedure,public :: show => showPlotClass
        procedure,public :: pdf  => pdfPlotClass
        procedure,public :: png  => pngPlotClass
    end type
contains

! ==========================================================
subroutine setLayoutPlotClass(this,h_v_size)
    class(Plot_),intent(inout) :: this
    integer(int32),intent(in) :: h_v_size(1:2)

    if(allocated(this%subplot) )then
        deallocate(this%subplot)
    endif
    allocate(this%subplot(h_v_size(1),h_v_size(2)) )

end subroutine
! ==========================================================


! ==========================================================
subroutine showPlotClass(this,name)
    class(Plot_),intent(in) :: this
    character(*),intent(in) :: name
    integer(int32) :: i,j,k
    type(IO_) :: f
    character(:),allocatable :: post_fix

    do i=1,size(this%subplot,1)
        do j=1,size(this%subplot,2)
            if(associated(this%subplot(i,j)%x) .and. associated(this%subplot(i,j)%y))then
                call f%open("Plot_"+str(i)+"_"+str(j)+".txt","w")
                do k=1,size(this%subplot(i,j)%x)
                    write(f%fh,*) this%subplot(i,j)%x(k),this%subplot(i,j)%y(k)
                enddo
                call f%close()
            endif
        enddo
    enddo

    call f%open(name+".gp","w")
    call f%write("unset key")
    call f%write('set xtics font "Times, 16"')
    call f%write('set ytics font "Times, 16"')
    call f%write('set key font "Times, 16"')
    call f%write("set multiplot layout "+str(size(this%subplot,1))+","+str(size(this%subplot,2)))
    do i=1,size(this%subplot,1)
        do j=1,size(this%subplot,2)
            if(allocated(this%subplot(i,j)%xr))then
                call f%write("set xr["+str(this%subplot(i,j)%xr(1))+":"+str(this%subplot(i,j)%xr(2))+"]")
            endif

            if(allocated(this%subplot(i,j)%yr))then
                call f%write("set yr["+str(this%subplot(i,j)%yr(1))+":"+str(this%subplot(i,j)%yr(2))+"]")
            endif

            if(associated(this%subplot(i,j)%x) .and. associated(this%subplot(i,j)%y))then
                if(allocated(this%subplot(i,j)%xlabel))then
                    call f%write("set xlabel '"+this%subplot(i,j)%xlabel+"' font 'Times, 16'")
                endif
                if(allocated(this%subplot(i,j)%ylabel))then
                    call f%write("set ylabel '"+this%subplot(i,j)%ylabel+"' font 'Times, 16'")
                endif
                if(allocated(this%subplot(i,j)%name ))then
                    post_fix = " title '"+this%subplot(i,j)%name+"'"
                else
                    post_fix = " notitle "
                endif
                write(f%fh,*)"plot "+"'Plot_"+str(i)+"_"+str(j)+".txt' w l "+post_fix
            elseif(allocated(this%subplot(i,j)%comment))then
                write(f%fh,*)"plot 0 w p ps 0 title '"+this%subplot(i,j)%comment+"'"
            else
                write(f%fh,*)"plot 0 w p ps 0 title 'No data'"
            endif
            if(allocated(this%subplot(i,j)%xr))then
                call f%write("unset xr")
            endif

            if(allocated(this%subplot(i,j)%yr))then
                call f%write("unset yr")
            endif
        enddo
    enddo
    call f%write("unset multiplot")
    call f%close()
end subroutine
! ==========================================================


! ==========================================================
subroutine pdfPlotClass(this,name)
    class(Plot_),intent(in) :: this
    character(*),intent(in) :: name
    character(:),allocatable :: command
    type(IO_) :: f


    call this%show(name)
    call f%open(name+"_pdf.gp")
    command = "set terminal pdf enhanced"
    call f%write(command)
    
    call f%write('set encoding utf8')
    if(size(this%subplot,1)>=size(this%subplot,2))then
        call f%write('set term pdfcairo size 21.0cm,29.7cm font "Times, 16" enhanced')
    else
        call f%write('set term pdfcairo size 29.7cm,21.0cm font "Times, 16" enhanced')
    endif
    call f%write('set output "'+name+'.pdf"')
    !call f%write('set tmargin 5.0')
    !call f%write('set bmargin 0.0')
    !call f%write('set rmargin 10.0')
    !call f%write('set lmargin 10.0')
    call f%write('set grid')
    

    call f%close()

    call system("cat "+name+".gp >> "+name+"_pdf.gp")

    call system("gnuplot "+name+"_pdf.gp")

end subroutine
! ==========================================================


! ==========================================================
subroutine pngPlotClass(this,name)
    class(Plot_),intent(in) :: this
    character(*),intent(in) :: name
    character(:),allocatable :: command
    type(IO_) :: f


    call this%show(name)
    call f%open(name+"_png.gp")
    
    call f%write('set encoding utf8')
    if(size(this%subplot,1)>=size(this%subplot,2))then
        call f%write('set terminal pngcairo size 1080, 1920')
    else
        call f%write('set terminal pngcairo size 1920, 1080')
    endif
    call f%write('set output "'+name+'.png"')
    !call f%write('set tmargin 5.0')
    !call f%write('set bmargin 0.0')
    !call f%write('set rmargin 10.0')
    !call f%write('set lmargin 10.0')
    call f%write('set grid')
    

    call f%close()

    call system("cat "+name+".gp >> "+name+"_png.gp")

    call system("gnuplot "+name+"_png.gp")

end subroutine
! ==========================================================

end module PlotClass