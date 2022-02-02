module EarthClass
    use sim
    implicit none

    type :: TimeZoneList_
        type(String_) :: ID(69)  
        real(real64) :: offset(69)
    contains
        procedure,public :: init => initTimeZoneList
    end type

    type :: Earth_
        real(real64) :: AirPressure = 101.325d0 ! kPa
        real(real64) :: AxisTilt = 23.4392811d0 ! deg. (https://en.wikipedia.org/wiki/Earth)
        real(real64) :: StandardGravity = 9.80665d0 ! m/s^2

        ! my position and timezone
        real(real64) :: MyPosition(2) = [36.380d0, 140.470d0] ! Ibaraki Pref. JAPAN 
        real(real64) :: MyTimeZonePosition=135.0d0! JAPAN
        character(:),allocatable :: TimeZone
        character(:),allocatable :: TimeZoneName
        real(real64) :: TimeZoneOffset
        
        ! my time
        integer(int32) :: Year  = 2022
        integer(int32) :: Month = 1
        integer(int32) :: Day   = 1
        integer(int32) :: Hour  = 0
        integer(int32) :: Minute= 0
        integer(int32) :: Second= 0
        integer(int32) :: Day_Per_Month(0:13) = [0,31,28,31,30,31,30,31,31,30,31,30,31,31] 

    contains
        procedure,public :: init => initEarth
        procedure,public :: setTime => setTimeEarthClass
        procedure,public :: setPosition => setPositionEarthClass
        procedure,public :: setTimeZone => setTimeZoneEarthClass
        procedure,public :: getSunTime => getSunTimeEarthClass    
        procedure,public :: getSunPosition => getSunPositionEarthClass
        procedure,public :: getTimeZone => getTimeZoneEarthClass
        procedure,public :: getTimeZoneOffset => getTimeZoneOffsetEarthClass
    end type
contains    
subroutine initTimeZoneList(this)
    class(TimeZoneList_),intent(inout) :: this

        this%id(1)%all = "Africa/Johannesburg"
        this%id(2)%all = "Africa/Lagos"
        this%id(3)%all = "Africa/Windhoek"
        this%id(4)%all = "America/Adak"
        this%id(5)%all = "America/Anchorage"
        this%id(6)%all = "America/Argentina_Buenos_Aires"
        this%id(7)%all = "America/Bogota"
        this%id(8)%all = "America/Caracas"
        this%id(9)%all = "America/Chicago"
        this%id(10)%all = "America/Denver"
        this%id(11)%all = "America/Godthab"
        this%id(12)%all = "America/Guatemala"
        this%id(13)%all = "America/Halifax"
        this%id(14)%all = "America/Los_Angeles"
        this%id(15)%all = "America/Montevideo"
        this%id(16)%all = "America/New_York"
        this%id(17)%all = "America/Noronha"
        this%id(18)%all = "America/Phoenix"
        this%id(19)%all = "America/Santiago"
        this%id(20)%all = "America/Santo_Domingo"
        this%id(21)%all = "America/St_Johns"
        this%id(22)%all = "Asia/Baghdad"
        this%id(23)%all = "Asia/Baku"
        this%id(24)%all = "Asia/Beirut"
        this%id(25)%all = "Asia/Dhaka"
        this%id(26)%all = "Asia/Dubai"
        this%id(27)%all = "Asia/Irkutsk"
        this%id(28)%all = "Asia/Jakarta"
        this%id(29)%all = "Asia/Kabul"
        this%id(30)%all = "Asia/Kamchatka"
        this%id(31)%all = "Asia/Karachi"
        this%id(32)%all = "Asia/Kathmandu"
        this%id(33)%all = "Asia/Kolkata"
        this%id(34)%all = "Asia/Krasnoyarsk"
        this%id(35)%all = "Asia/Omsk"
        this%id(36)%all = "Asia/Rangoon"
        this%id(37)%all = "Asia/Shanghai"
        this%id(38)%all = "Asia/Tehran"
        this%id(39)%all = "Asia/Tokyo"
        this%id(40)%all = "Asia/Vladivostok"
        this%id(41)%all = "Asia/Yakutsk"
        this%id(42)%all = "Asia/Yekaterinburg"
        this%id(43)%all = "Atlantic/Azores"
        this%id(44)%all = "Atlantic/Cape_Verde"
        this%id(45)%all = "Australia/Adelaide"
        this%id(46)%all = "Australia/Brisbane"
        this%id(47)%all = "Australia/Darwin"
        this%id(48)%all = "Australia/Eucla"
        this%id(49)%all = "Australia/Lord_Howe"
        this%id(50)%all = "Australia/Sydney"
        this%id(51)%all = "Etc/GMT12"
        this%id(52)%all = "Europe/Berlin"
        this%id(53)%all = "Europe/London"
        this%id(54)%all = "Europe/Moscow"
        this%id(55)%all = "Pacific/Apia"
        this%id(56)%all = "Pacific/Auckland"
        this%id(57)%all = "Pacific/Chatham"
        this%id(58)%all = "Pacific/Easter"
        this%id(59)%all = "Pacific/Gambier"
        this%id(60)%all = "Pacific/Honolulu"
        this%id(61)%all = "Pacific/Kiritimati"
        this%id(62)%all = "Pacific/Majuro"
        this%id(63)%all = "Pacific/Marquesas"
        this%id(64)%all = "Pacific/Norfolk"
        this%id(65)%all = "Pacific/Noumea"
        this%id(66)%all = "Pacific/Pago_Pago"
        this%id(67)%all = "Pacific/Pitcairn"
        this%id(68)%all = "Pacific/Tongatapu"
        this%id(69)%all = "UTC"

    this%offset(1) =dble( 2)
    this%offset(2) =dble( 1)
    this%offset(3) =dble( 1)
    this%offset(4) =dble( -10)
    this%offset(5) =dble( -9)
    this%offset(6) =dble( -3)
    this%offset(7) =dble( -5)
    this%offset(8) =dble( -4.5)
    this%offset(9) =dble( -6)
    this%offset(10) =dble( -7)
    this%offset(11) =dble( -3)
    this%offset(12) =dble( -6)
    this%offset(13) =dble( -4)
    this%offset(14) =dble( -8)
    this%offset(15) =dble( -3)
    this%offset(16) =dble( -5)
    this%offset(17) =dble( -2)
    this%offset(18) =dble( -7)
    this%offset(19) =dble( -4)
    this%offset(20) =dble( -4)
    this%offset(21) =dble( -3.5)
    this%offset(22) =dble( 3)
    this%offset(23) =dble( 4)
    this%offset(24) =dble( 2)
    this%offset(25) =dble( 6)
    this%offset(26) =dble( 4)
    this%offset(27) =dble( 9)
    this%offset(28) =dble( 7)
    this%offset(29) =dble( 4.5)
    this%offset(30) =dble( 12)
    this%offset(31) =dble( 5)
    this%offset(32) =dble( 5.75)
    this%offset(33) =dble( 5.5)
    this%offset(34) =dble( 8)
    this%offset(35) =dble( 7)
    this%offset(36) =dble( 6.5)
    this%offset(37) =dble( 8)
    this%offset(38) =dble( 3.5)
    this%offset(39) =dble( 9)
    this%offset(40) =dble( 11)
    this%offset(41) =dble( 10)
    this%offset(42) =dble( 6)
    this%offset(43) =dble( -1)
    this%offset(44) =dble( -1)
    this%offset(45) =dble( 9.5)
    this%offset(46) =dble( 10)
    this%offset(47) =dble( 9.5)
    this%offset(48) =dble( 8.75)
    this%offset(49) =dble( 10.5)
    this%offset(50) =dble( 10)
    this%offset(51) =dble( -12)
    this%offset(52) =dble( 1)
    this%offset(53) =dble( 0)
    this%offset(54) =dble( 4)
    this%offset(55) =dble( 13)
    this%offset(56) =dble( 12)
    this%offset(57) =dble( 12.75)
    this%offset(58) =dble( -6)
    this%offset(59) =dble( -9)
    this%offset(60) =dble( -10)
    this%offset(61) =dble( 14)
    this%offset(62) =dble( 12)
    this%offset(63) =dble( -9.5)
    this%offset(64) =dble( 11.5)
    this%offset(65) =dble( 11)
    this%offset(66) =dble( -11)
    this%offset(67) =dble( -8)
    this%offset(68) =dble( 13)
    this%offset(69) =dble( 0)

end subroutine

! ###############################################################
subroutine initEarth(this,Now,time,DateTime) 
    class(Earth_),intent(inout) :: this
    integer(int32),optional,intent(in) :: DateTime(6) ! Year, Month, Day, Hour, Minute, Second
    logical,optional,intent(in) :: Now
    type(Time_),optional,intent(in) :: time
    character(:),allocatable :: d_time

    if(present(Now))then
        call this%setTime(Now=Now)
    elseif(present(DateTime))then
        call this%setTime(DateTime=DateTime)
    elseif(present(time))then
        call this%setTime(time=time)
    else
        call this%setTime(Now=.true.)
    endif

    this%TimeZoneName="Asia/Tokyo" ! JAPAN
    this%MyPosition(1:2) = [36.380d0, 140.470d0] ! Ibaraki Pref. JAPAN 
    this%MyTimeZonePosition=135.0d0! JAPAN
    this%TimeZone="+0900" ! JAPAN

end subroutine


! ###############################################################
subroutine setTimeEarthClass(this,Now,time,DateTime) 
    class(Earth_),intent(inout) :: this
    integer(int32),optional,intent(in) :: DateTime(6) ! Year, Month, Day, Hour, Minute, Second
    logical,optional,intent(in) :: Now
    type(Time_),optional,intent(in) :: time
    type(Time_) :: internal_time
    character(:),allocatable :: d_time

    if(present(DateTime) )then
        this%Year   = DateTime(1)
        this%Month  = DateTime(2)
        this%Day    = DateTime(3)
        this%Hour   = DateTime(4)
        this%Minute = DateTime(5)
        this%Second = DateTime(6)
    endif

    if(present(Now) )then
        if(Now)then
            d_time =  internal_time%DateAndTime()
            this%Year   = fint(d_time(1:4))
            this%Month  = fint(d_time(5:6))
            this%Day    = fint(d_time(7:8))
            this%Hour   = fint(d_time(9:10))
            this%Minute = fint(d_time(11:12))
            this%Second = fint(d_time(13:14))
        endif
    endif

    if(present(time) )then
        this%Year   = fint(time%date(1:4))
        this%Month  = fint(time%date(5:6))
        this%Day    = fint(time%date(7:8))
        this%Hour   = fint(time%time(1:2))
        this%Minute = fint(time%time(3:4))
        this%Second = fint(time%time(5:6))
        this%timezone = time%zone
    endif
end subroutine
! ###############################################################

! ###############################################################
subroutine setTimeZoneEarthClass(this,name) 
    class(Earth_),intent(inout) :: this
    character(*),intent(in) :: name
    type(TimeZoneList_)::TimeZoneList

    this%MyTimeZonePosition= this%getTimeZoneOffset(name=name)*15.0d0
    this%TimeZoneOffset=this%getTimeZoneOffset(name=name)
    
end subroutine

! ###############################################################

! ###############################################################
subroutine setPositionEarthClass(this,MyPosition,MyTimeZonePosition,MyTimeZone) 
    class(Earth_),intent(inout) :: this
    real(real64),optional,intent(in) :: MyPosition(2)
    real(real64),optional,intent(in) :: MyTimeZonePosition
    character(5),optional,intent(in) :: MyTimeZone

    this%MyPosition(1:2) = MyPosition!e.g.[36.380d0, 140.470d0] ! Ibaraki Pref. JAPAN 
    this%MyTimeZonePosition=MyTimeZonePosition!e.g.[135.0d0]! JAPAN
    this%TimeZone=MyTimeZone! e.g. "+0900" ! JAPAN
    

end subroutine
! ###############################################################

function getSunTimeEarthClass(this,Now) result(times)
    !http://k-ichikawa.blog.enjoy.jp/etc/HP/js/sunShineAngle/ssa.html
    class(Earth_),intent(inout) :: this
    integer(int32),intent(in) :: Now(6) ! Year, Month, Day, Hour, Minute, Second
    real(real64) :: times(2) 
    type(Math_)  :: math
    real(real64) :: delta
    real(real64) :: omega
    real(real64) :: J, e
    real(real64) :: t,TT,h, A,TT1,TT2,t1,t2,sinA,cosA,Ts,buf
    integer(int32) :: total_days

    this%Year   = Now(1)
    this%Month  = Now(2)
    this%Day    = Now(3)
    this%Hour   = Now(4)
    this%Minute = Now(5)
    this%Second = Now(6)

    total_days = sum(this%Day_Per_Month(1:this%Month-1)) + this%Day -1
    omega = 2.0d0*math%pi/365.0d0
    J     = dble(total_days)+0.50d0
    
    delta = 0.33281d0 - 22.984d0*cos(omega*J ) - 0.34990d0* cos(2.0d0*omega*J )&
     - 0.13980d0*cos(3.0d0*omega*J ) + 3.7872d0* sin(omega*J ) &
     + 0.03250d0* sin(2.0d0*omega*J ) + 0.07187d0* sin(3.0d0*omega*J )


    e = 0.0072d0*cos(omega*J ) - 0.0528d0*cos(2.0d0*omega*J ) - 0.0012d0*cos(3.0d0*omega*J )&
        - 0.1229d0* sin(omega*J ) - 0.1565d0* sin(2.0d0*omega*J ) &
        - 0.0041d0* sin(3.0d0*omega*J )
    
    Ts = dble(this%Hour) + dble(this%Minute)/60.0d0 + dble(this%Second)/3600.0d0
    
    TT = Ts + (this%MyPosition(2) - this%MyTimeZonePosition)/15.0d0 + e
    
    t = 15.0d0*TT - 180.0d0

    buf = sin( radian(this%MyPosition(1)) )*sin( radian(delta) ) &
    + cos(  radian(this%MyPosition(1)) )*cos( radian(delta) )*cos( radian(t) )
    h = asin(buf)
    
    sinA = cos(radian(delta))*sin(radian(t))/cos(h)
    cosA = (sin(h)*sin( radian(this%MyPosition(1)) )&
     - sin(radian(delta)))/cos(h)/cos( radian(this%MyPosition(1)) )
    A = atan2(sinA, cosA ) + math%pi

    t = acos(-tan(radian(delta) )*tan(  radian(this%MyPosition(1)) ))
    TT1 = (-degrees(t) + 180.0d0)/15.0d0
    t1 = TT1 - (this%MyPosition(2) - 135.0d0)/15.0d0 - e

    TT2 = ( degrees(t) + 180.0d0)/15.0d0
    t2 = TT2 - (this%MyPosition(2) - 135.0d0)/15.0d0 - e

    times(1) = t1 ! 日の出(hr)
    times(2) = t2 ! 日没(hr)

end function

function getSunPositionEarthClass(this,Now,DateTime,dt) result(angles)
    !http://k-ichikawa.blog.enjoy.jp/etc/HP/js/sunShineAngle/ssa.html
    class(Earth_),intent(inout) :: this
    integer(int32),optional,intent(in) :: DateTime(6) ! Year, Month, Day, Hour, Minute, Second
    logical,optional,intent(in) :: Now
    integer(int32),optional,intent(in) :: dt
    real(real64) :: angles(2) 
    type(Math_)  :: math
    real(real64) :: delta
    real(real64) :: omega
    real(real64) :: J, e
    real(real64) :: t,TT,h, A,TT1,TT2,t1,t2,sinA,cosA,Ts,buf
    integer(int32) :: total_days

    type(Time_) :: time

    character(:),allocatable :: d_time

    if(present(DateTime) )then
        this%Year   = DateTime(1)
        this%Month  = DateTime(2)
        this%Day    = DateTime(3)
        this%Hour   = DateTime(4)
        this%Minute = DateTime(5)
        this%Second = DateTime(6)
    endif

    if(present(Now) )then
        if(Now)then
            d_time =  time%DateAndTime()
            this%Year   = fint(d_time(1:4))
            this%Month  = fint(d_time(5:6))
            this%Day    = fint(d_time(7:8))
            this%Hour   = fint(d_time(9:10))
            this%Minute = fint(d_time(11:12))
            this%Second = fint(d_time(13:14))
        endif
    endif

    if(present(dt) )then
        ! dt (sec.)
        this%Second = this%Second + dt
        do 
            if(this%Second>=60)then
                this%Second = this%Second - 60
                this%Minute = this%Minute+1
            else
                exit
            endif
        enddo

        do 
            if(this%Minute>=60)then
                this%Minute = this%Minute - 60
                this%hour = this%hour+1
            else
                exit
            endif
        enddo
        

        do 
            if(this%hour>=24)then
                this%hour = this%hour - 24
                this%day = this%day+1
            else
                exit
            endif
        enddo


        do 
            if(this%day>  this%Day_Per_Month(this%month) )then
                this%day = this%day - this%Day_Per_Month(this%month)
                this%Month = this%Month+1
            else
                exit
            endif
        enddo


        do 
            if(this%month >= 13 )then
                this%month = this%month - 12
                this%Year = this%Year + 1
            else
                exit
            endif
        enddo
    endif

    total_days = sum(this%Day_Per_Month(1:this%Month-1)) + this%Day -1
    omega = 2.0d0*math%pi/365.0d0
    J     = dble(total_days)+0.50d0
    
    delta = 0.33281d0 - 22.984d0*cos(omega*J ) - 0.34990d0* cos(2.0d0*omega*J )&
     - 0.13980d0*cos(3.0d0*omega*J ) + 3.7872d0* sin(omega*J ) &
     + 0.03250d0* sin(2.0d0*omega*J ) + 0.07187d0* sin(3.0d0*omega*J )


    e = 0.0072d0*cos(omega*J ) - 0.0528d0*cos(2.0d0*omega*J ) - 0.0012d0*cos(3.0d0*omega*J )&
        - 0.1229d0* sin(omega*J ) - 0.1565d0* sin(2.0d0*omega*J ) &
        - 0.0041d0* sin(3.0d0*omega*J )
    
    Ts = dble(this%Hour) + dble(this%Minute)/60.0d0 + dble(this%Second)/3600.0d0
    
    TT = Ts + (this%MyPosition(2) - this%MyTimeZonePosition)/15.0d0 + e
    
    t = 15.0d0*TT - 180.0d0

    buf = sin( radian(this%MyPosition(1)) )*sin( radian(delta) ) &
    + cos(  radian(this%MyPosition(1)) )*cos( radian(delta) )*cos( radian(t) )
    h = asin(buf)
    
    sinA = cos(radian(delta))*sin(radian(t))/cos(h)
    cosA = (sin(h)*sin( radian(this%MyPosition(1)) )&
     - sin(radian(delta)))/cos(h)/cos( radian(this%MyPosition(1)) )
    A = atan2(sinA, cosA ) + math%pi

    t = acos(-tan(radian(delta) )*tan(  radian(this%MyPosition(1)) ))
    TT1 = (-degrees(t) + 180.0d0)/15.0d0
    t1 = TT1 - (this%MyPosition(2) - 135.0d0)/15.0d0 - e

    TT2 = ( degrees(t) + 180.0d0)/15.0d0
    t2 = TT2 - (this%MyPosition(2) - 135.0d0)/15.0d0 - e

    angles(1) = degrees(A) ! 方位角(deg.)
    angles(2) = degrees(H) ! 太陽高度(deg.)

    if(Ts < t1)then
        angles(:) = 0.0d0
        return
    endif

    if(Ts > t2)then
        angles(:) = 0.0d0
        return
    endif


end function
! ################################################################


! ################################################################
function getTimeZoneEarthClass(this,name) result(timezone)
    class(Earth_),intent(in) :: this
    character(*),intent(in) :: name
    type(TimeZoneList_) :: TimeZoneList
    character(:),allocatable :: timezone
    integer(int32) :: i
    
    timezone = ""
    call TimeZoneList%init()
    do i=1,size(TimeZoneList%ID,1)
        if(index(TimeZoneList%ID(i)%all,trim(name) )==0 )then
            cycle
        else
            timezone = trim(TimeZoneList%ID(i)%all)
            return
        endif
    enddo

    print *, "[ERROR] no such time-zone name as",name
    stop
    
end function
! ################################################################

! ################################################################
function getTimeZoneOffsetEarthClass(this,name) result(timezoneoffset)
    class(Earth_),intent(in) :: this
    character(*),intent(in) :: name
    type(TimeZoneList_) :: TimeZoneList
    real(real64) :: timezoneoffset
    integer(int32) :: i
    timezoneoffset = 0

    call TimeZoneList%init()
    do i=1,size(TimeZoneList%ID,1)
        if(index(TimeZoneList%ID(i)%all,trim(name) )==0 )then
            cycle
        else
            timezoneoffset = TimeZoneList%offset(i)
            return
        endif
    enddo

    print *, "[ERROR] no such time-zone name as",name
    stop

end function
! ################################################################

end module 