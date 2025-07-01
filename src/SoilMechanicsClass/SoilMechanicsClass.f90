module SoilMechanicsClass
    use fem
    implicit none

    type :: SoilMechanics_
        ! metaparameters for sweden method (slope stability)
        integer(int32) :: sweden_method_num_spatial_div=100
        integer(int32) :: num_radius_division=100

    contains
        procedure,public :: sweden_method => sweden_method_SoilMechanics
        procedure,public :: get_Fs_of_slices => get_Fs_of_slices_SoilMechanics
        procedure,public :: get_slice => get_slice_SoilMechanics
        procedure,public :: show_slope_and_circle => show_slope_and_circle_SoilMech
        procedure,public :: get_volume_of_slice => get_volume_of_slice_SoilMech
        procedure,public :: get_bottom_angle_of_slice => get_bottom_angle_of_slice_SM
    end type
contains

function sweden_method_SoilMechanics(this,name,slope_angle,slope_height,density,&
        num_division,c,phi) result(ret)
    character(*),intent(in) :: name
    class(SoilMechanics_),intent(in) :: this
    real(real64),intent(in) :: slope_angle,slope_height,density,c,phi
    integer(int32),intent(in) :: num_division 
    real(real64) :: ret

    type(IO_) :: f,gp
    real(real64) :: circle_center(1:2), circle_radius
    integer(int32) ::  x_n, y_n, y_idx, x_idx

    real(real64),allocatable :: x_list(:),y_list(:)
    real(real64) :: W_i,ith_slice(1:4,1:2),vol,Fs,tau,Fs_1,Fs_2,l,alpha,&
        search_range_x(1:2),search_range_y(1:2),max_fs_value,tan_a

    real(real64) :: solution(1:4)

    max_fs_value = 5.0d0 ! 安全率5より大きい場合は5
    ! 法尻を原点とする．

    ! 円中心(y>=0)
    ! slope_angle   = 30.0d0 ! deg.
    ! slope_height  = 5.0d0 ! m
    ! density       = 1.80d0 ! t/m^3
    ! num_division  = 20
    ! c   = 40.0d0 ! kPa
    ! phi = 1.0d0 ! deg.

    ! 安全率探索範囲
    tan_a = tan(radian(slope_angle))
    search_range_x = [-slope_height/tan_a*3.0d0,slope_height/tan_a*3.0d0] ! x
    search_range_y = [0.0d0,3.0d0*slope_height] ! y

    
    
    x_n = this%sweden_method_num_spatial_div
    y_n = this%sweden_method_num_spatial_div

    x_list = linspace(search_range_x,x_n)
    y_list = linspace(search_range_y,y_n)

    call f%open(name+"_Fs.txt","w")
    
    solution(1:4) = 100000.0d0
    do x_idx=1,x_n
        do y_idx=1,y_n
            circle_center = [x_list(x_idx), y_list(y_idx)]
            Fs = this%get_Fs_of_slices(circle_center, slope_angle, slope_height,c,phi,num_division,circle_radius,density)        
            Fs = abs(Fs)
            if(Fs > max_fs_value)then
                Fs = max_fs_value
            elseif(Fs < 0.0d0)then
                Fs = 0.0d0
            endif

            if(solution(4) > Fs)then
                solution(1:2) = circle_center(1:2)
                solution(3)   = circle_radius
                solution(4)   = Fs
            endif

            write(f%fh,*) circle_center(1:2), Fs
            call f%flush()
        enddo
        call f%write(" ")
    enddo

    call f%close()

    circle_center = solution(1:2)
    circle_radius = solution(3)
    Fs = solution(4)
    ret = Fs
    call this%show_slope_and_circle(name,circle_center,circle_radius,slope_angle,slope_height,num_division)
    
    ! gnuplot
    call gp%open(name+"_geo.gp")
    call gp%write("set terminal svg")
    call gp%write("plot '"+name+"_divisions.txt' u 1:2 w l")
    call gp%write("replot '"+name+"_slope_and_circle.txt' u 1:2 w l")
    call gp%write("set output '"+name+"_geo.svg'")
    call gp%write("replot")
    call gp%write("exit")
    call gp%close()

    call system("gnuplot "+name+"_geo.gp")

    call gp%open(name+"_cont.gp")
    call gp%write("set terminal svg")
    call gp%write("set pm3d")
    call gp%write("set pm3d map")
    call gp%write("splot '"+name+"_min_Fs_value.txt' u 1:2:3 w pm3d")
    call gp%write("set output '"+name+"_cont.svg'")
    call gp%write("replot")
    call gp%write("exit")
    call gp%close()

    call system("gnuplot "+name+"_cont.gp")
    
end function




function get_Fs_of_slices_SoilMechanics(this,circle_center,&
     slope_angle, slope_height,c,phi,num_division, opt_radius,density) result(Fs)
    class(SoilMechanics_),intent(in) :: this
    type(IO_) :: f
    real(real64),intent(in) :: circle_center(1:2), slope_angle, slope_height,c,phi,density
    integer(int32),intent(in) :: num_division 


    real(real64) :: circle_radius,opt_radius
    real(real64) :: W_i,ith_slice(1:4,1:2),vol,Fs,tau,Fs_1,Fs_2,l,alpha
    real(real64),allocatable :: radius_list(:),Fs_list(:)
    
    integer(int32) :: n,j

    n = this%num_radius_division
    ! find minimum Fs by changing radius
    radius_list = linspace([slope_height*2/dble(n),slope_height*2],n)

    Fs = 100000000.0d0
    Fs_list = Fs*ones(n)
    

    !$OMP parallel do private(Fs_1,Fs_2,circle_radius,ith_slice,vol,alpha,l,W_i)
    do j=1,n
        Fs_1 = 0.0d0
        Fs_2 = 0.0d0
        do i_i=1,num_division
            circle_radius = radius_list(j)
            ith_slice = this%get_slice(num_division,circle_center,circle_radius,slope_angle,slope_height,i_i)
            
            vol = this%get_volume_of_slice(ith_slice)
            alpha = this%get_bottom_angle_of_slice(ith_slice)
            l     = norm(ith_slice(3,1:2)-ith_slice(2,1:2))
            W_i = 9.810d0*density*vol

            Fs_1 = Fs_1 + c*l + tan(radian(phi))*W_i*cos(alpha)
            Fs_2 = Fs_2 + W_i * sin(alpha)
        enddo
        Fs_list(j) = Fs_1/Fs_2
        ! if(Fs > Fs_1/Fs_2 )then
        !     Fs  = Fs_1/Fs_2
        !     opt_radius = radius_list(j)
        ! endif
    enddo    

    Fs = minval(Fs_list)
    opt_radius = radius_list(minvalID(Fs_list))

end function



function get_slice_SoilMechanics(this,num_division,circle_center,circle_radius,slope_angle,slope_height,idx) result(ith_slice)
    class(SoilMechanics_),intent(in) :: this
    real(real64),intent(in) :: circle_center(1:2), slope_angle, slope_height,circle_radius
    integer(int32),intent(in) :: num_division,idx
    real(real64) :: ith_slice(1:4,1:2)
    real(real64) :: b_i,x_min,x_max,ds,tan_a,r,&
        x_bar,f1,f2,x_c,y_c,k,d1,d2,a_bar,b_bar,c_bar,m,tot_len,dx
    real(real64),allocatable :: x_cross(:)
    
    integer(int32) :: n_cross(1:3)
    real(real64),allocatable :: min_max_val_arr(:)


    x_cross = zeros(6)
    x_c = circle_center(1)
    y_c = circle_center(2)
    tan_a = tan(radian(slope_angle))
    m = tan_a
    r = circle_radius
    d1 = y_c
    d2 = abs(y_c - slope_height)
    ith_slice(:,:) = 0.0d0


    ! 中心がslope地盤面より上になければ，ith_slice = 0.0d0
    if(circle_center(1) <= 0.0d0)then
        if(circle_center(2) < 0.0d0)then
            return
        endif
    elseif(circle_center(1) <= slope_height/tan_a)then
        if(circle_center(2) < tan_a*circle_center(1))then
            return
        endif
    else
        if(circle_center(2) < slope_height)then
            return
        endif
    endif
    ! 全部の交点を求める．
    ! bottom lineとの交点数
    if(d1 < r)then
        n_cross(1) = 2
        
        x_cross(1) = - sqrt(r*r - d1*d1) + x_c
        x_cross(2) =   sqrt(r*r - d1*d1) + x_c
    elseif(d1 == r)then
        n_cross(1) = 1
        
        x_cross(1) = x_c
        x_cross(2) = x_c
    else
        n_cross(1) = 0

        x_cross(1) = 0.0d0
        x_cross(2) = 0.0d0
    endif
    ! slope lineとの交点数

    a_bar = (1.0d0+m**2)
    b_bar = -2.0d0*x_c  - 2.0d0*y_c*m
    c_bar = x_c**2 + y_c**2 - r**2
    if(b_bar**2 - 4.0d0*a_bar*c_bar > 0.0d0)then
        n_cross(2) = 2

        x_cross(3) = (- b_bar - sqrt(b_bar**2 - 4.0d0*a_bar*c_bar))/(2.0d0*a_bar)
        x_cross(4) = (- b_bar + sqrt(b_bar**2 - 4.0d0*a_bar*c_bar))/(2.0d0*a_bar)
    elseif(b_bar**2 - 4.0d0*a_bar*c_bar == 0.0d0)then
        n_cross(2) = 1

        x_cross(3) = (- b_bar)/(2.0d0*a_bar)
        x_cross(4) = (- b_bar)/(2.0d0*a_bar)
    else
        n_cross(2) = 0

        x_cross(3) = 0.0d0
        x_cross(4) = 0.0d0
    endif

    ! top lineとの交点数
    if(d2 < r)then
        n_cross(3) = 2
        
        x_cross(5) = - sqrt(r*r - d2*d2) + x_c
        x_cross(6) =   sqrt(r*r - d2*d2) + x_c
    elseif(d2 == r)then
        n_cross(3) = 1
        
        x_cross(5) = x_c
        x_cross(6) = x_c
    else
        n_cross(3) = 0

        x_cross(5) = 0.0d0
        x_cross(6) = 0.0d0
    endif

    min_max_val_arr = zeros(0)
    if(n_cross(1)/=0)then
        if(x_cross(1)<=0.0d0)then
            min_max_val_arr = min_max_val_arr // [x_cross(1)]
        endif
        if(x_cross(2)<=0.0d0)then
            min_max_val_arr = min_max_val_arr // [x_cross(2)]
        endif
    endif

    if(n_cross(2)/=0)then
        if(0.0d0 <= x_cross(3) .and. x_cross(3)  <= slope_height/tan_a)then
            min_max_val_arr = min_max_val_arr // [x_cross(3)]
        endif
        if(0.0d0 <= x_cross(4) .and. x_cross(4)  <= slope_height/tan_a)then
            min_max_val_arr = min_max_val_arr // [x_cross(4)]
        endif
    endif


    if(n_cross(3)/=0)then
        if( x_cross(5) >= slope_height/tan_a)then
            min_max_val_arr = min_max_val_arr // [x_cross(5)]
        endif
        if( x_cross(6) >= slope_height/tan_a)then
            min_max_val_arr = min_max_val_arr // [x_cross(6)]
        endif
    endif

    x_min = minval(min_max_val_arr)
    x_max = maxval(min_max_val_arr)

    tot_len = x_max - x_min

    dx = tot_len/dble(num_division)

    ith_slice(:,:) = 0.0d0
    ! 左上
    ith_slice(1,1) = x_min + dx*(idx-1)
    if(ith_slice(1,1) <= 0.0d0)then
        ith_slice(1,2) = 0.0d0
    elseif(ith_slice(1,1) <= slope_height/tan_a)then
        ith_slice(1,2) = tan_a*ith_slice(1,1)
    else
        ith_slice(1,2) = slope_height
    endif
    ! 左下
    ith_slice(2,1) = x_min + dx*(idx-1) 
    ith_slice(2,2) = y_c - sqrt(r**2-(ith_slice(2,1)-x_c)**2)

    ! 右下
    ith_slice(3,1) = x_min + dx*(idx  )
    ith_slice(3,2) = y_c - sqrt(r**2-(ith_slice(3,1)-x_c)**2)

    ! 右上
    ith_slice(4,1) = x_min + dx*(idx  ) 
    if(ith_slice(4,1) <= 0.0d0)then
        ith_slice(4,2) = 0.0d0
    elseif(ith_slice(4,1) <= slope_height/tan_a)then
        ith_slice(4,2) = tan_a*ith_slice(4,1)
    else
        ith_slice(4,2) = slope_height
    endif

end function



subroutine show_slope_and_circle_SoilMech(this,name,circle_center,circle_radius,slope_angle,slope_height,num_division)

    class(SoilMechanics_),intent(in) :: this
    character(*),intent(in) :: name
    real(real64),intent(in) :: circle_center(1:2), slope_angle, slope_height,circle_radius
    real(real64) :: ith_slice(1:4,1:2)
    integer(int32),intent(in) :: num_division
    integer(int32) :: i,n
    real(real64) :: x,y,tan_a,dx
    type(Math_) :: math
    type(IO_) :: f
    n = 1000

    tan_a = tan(radian(slope_angle))
    call f%open(name+"_slope_and_circle.txt","w")
    ! slope
    dx = slope_height*6.0d0/n
    do i=1,n
        x = -dx*(i-1)
        y=0.0d0
        write(f%fh,*) x, y
    enddo
    dx = slope_height/(tan_a)/n
    do i=1,n
        x = dx*(i-1)
        y = tan_a*x
        write(f%fh,*) x, y
    enddo
    dx = slope_height*6.0d0/n
    do i=1,n
        x = slope_height/(tan_a)+dx*(i-1)
        y = slope_height
        write(f%fh,*) x, y
    enddo
    ! circle
    write(f%fh,*) " "
    write(f%fh,*) circle_center(1),circle_center(2)
    write(f%fh,*) " "
    do i=1,n
        x = circle_center(1) + circle_radius*cos(2.0d0*math%pi/dble(n-1)*(i-1))
        y = circle_center(2) + circle_radius*sin(2.0d0*math%pi/dble(n-1)*(i-1))
        write(f%fh,*) x, y
    enddo
    
    call f%close()

    
    call f%open(name+"_divisions.txt","w")
    do i_i=1,num_division
        ith_slice = this%get_slice(num_division,circle_center,circle_radius,slope_angle,slope_height,i_i)
        
        write(f%fh,*) ith_slice(1,1:2)
        write(f%fh,*) ith_slice(2,1:2)
        write(f%fh,*) ith_slice(3,1:2)
        write(f%fh,*) ith_slice(4,1:2)    
        write(f%fh,*) ith_slice(1,1:2)    
        write(f%fh,*) " "
    enddo
    call f%close()
    
    

end subroutine



function get_volume_of_slice_SoilMech(this,ith_slice) result(vol)
    class(SoilMechanics_),intent(in) :: this
    real(real64),intent(in) :: ith_slice(:,:)
    real(real64) :: vol,a,b,c,d,s

    a = norm(ith_slice(2,1:2)-ith_slice(1,1:2))
    b = norm(ith_slice(3,1:2)-ith_slice(2,1:2))
    c = norm(ith_slice(4,1:2)-ith_slice(3,1:2))
    d = norm(ith_slice(1,1:2)-ith_slice(4,1:2))

    s = (a+b+c)/2.0d0
    vol = sqrt(s*abs(s-a)*abs(s-b)*abs(s-c))


    s = (c+d+a)/2.0d0
    vol = vol + sqrt(s*abs(s-c)*abs(s-d)*abs(s-a))

end function


function get_bottom_angle_of_slice_SM(this,ith_slice) result(alpha)
    class(SoilMechanics_),intent(in) :: this
    real(real64),intent(in) :: ith_slice(:,:)
    real(real64) :: alpha
    ! alpha = atan2(dy/dx)
    alpha = atan2((ith_slice(3,2)-ith_slice(2,2)),(ith_slice(3,1)-ith_slice(2,1)))

end function



end module SoilMechanicsClass