module LightClass
    use fem
    implicit none

    type :: Light_
        character(200)   :: lighttype="sun"
        real(real64)     :: position(3)
        real(real64)     :: maxPPFD
        type(FEMDomain_) :: femdomain 
    contains
        procedure,public :: init => initLight
    end type
contains

! #################################
subroutine initLight(obj,config)
    class(Light_),intent(inout) :: obj
    character(*),optional,intent(in) :: config
    type(IO_) :: lightconf,f
    character(200) :: fn,conf,line
    integer(int32),allocatable :: buf(:)
    integer(int32) :: id,rmc,n,node_id,node_id2,elemid,blcount,i,j
    real(real64) :: loc(3)

    ! 節を生成するためのスクリプトを開く
    if(.not.present(config) .or. index(config,".json")==0 )then
        ! デフォルトの設定を生成
        print *, "New light-configuration >> lightconfig.json"
        call lightconf%open("lightconfig.json")
        write(lightconf%fh,*) '{'
        write(lightconf%fh,*) '   "type": "light",'
        write(lightconf%fh,*) '   "source": "sun",'
        write(lightconf%fh,*) '   "position_x": 0.00,'
        write(lightconf%fh,*) '   "position_y": 0.00,'
        write(lightconf%fh,*) '   "position_z": 1.00e+18,'
        write(lightconf%fh,*) '   "maxPPFD": 1000.00'
        write(lightconf%fh,*) '}'
        conf="lightconfig.json"
        call lightconf%close()
    else
        conf = trim(config)
    endif
    
    call lightconf%open(trim(conf))
    blcount=0
    do
        read(lightconf%fh,'(a)') line
        print *, trim(line)
        if( adjustl(trim(line))=="{" )then
            blcount=1
            cycle
        endif
        if( adjustl(trim(line))=="}" )then
            exit
        endif
        
        if(blcount==1)then
            
            if(index(line,"type")/=0 .and. index(line,"light")==0 )then
                print *, "ERROR: This config-file is not for light"
                return
            endif

            if(index(line,"source")/=0 )then
                ! 生育ステージ
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%lighttype
            endif


            if(index(line,"position_x")/=0 )then
                ! 種子の長さ
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%position(1)
            endif

            if(index(line,"position_y")/=0 )then
                ! 種子の長さ
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%position(2)
            endif


            if(index(line,"maxPPFD")/=0 )then
                ! 生育ステージ
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%maxPPFD
            endif

            cycle

        endif

    enddo
    call lightconf%close()

        


end subroutine
! #################################
end module