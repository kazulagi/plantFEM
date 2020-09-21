module FertilizerClass
    use fem
    implicit none

    type :: Fertilizer_
    
        ! total mass
        real(real64) :: total_kg = 0.0d0

        ! percentage
        real(real64) :: N_pct = 0.0d0
        real(real64) :: P_pct = 0.0d0
        real(real64) :: K_pct = 0.0d0
        real(real64) :: Ca_pct = 0.0d0
        real(real64) :: Mg_pct = 0.0d0
        real(real64) :: S_pct = 0.0d0
        !------------
        real(real64) :: Fe_pct = 0.0d0
        real(real64) :: Mn_pct = 0.0d0
        real(real64) :: B_pct = 0.0d0
        real(real64) :: Zn_pct = 0.0d0
        real(real64) :: Mo_pct = 0.0d0
        real(real64) :: Cu_pct = 0.0d0
        real(real64) :: Cl_pct = 0.0d0

        ! total nutorient
        real(real64) :: N_kg =0.0d0
        real(real64) :: P_kg =0.0d0 
        real(real64) :: K_kg =0.0d0 
        real(real64) :: Ca_kg=0.0d0
        real(real64) :: Mg_kg=0.0d0
        real(real64) :: S_kg =0.0d0
        real(real64) :: Fe_kg = 0.0d0
        real(real64) :: Mn_kg = 0.0d0
        real(real64) :: B_kg  = 0.0d0
        real(real64) :: Zn_kg = 0.0d0
        real(real64) :: Mo_kg = 0.0d0
        real(real64) :: Cu_kg = 0.0d0
        real(real64) :: Cl_kg = 0.0d0
    contains
        procedure :: init => initFertilizer
    end type
contains


! ######################################################
subroutine initFertilizer(obj,config)
    class(Fertilizer_),intent(inout) :: obj
    character(*),optional,intent(in) :: config
    type(IO_) :: fertconf
    character(200) :: fn,conf,line
    integer(int32) :: id,rmc,n,node_id,node_id2,elemid,blcount,i,j
    


    ! 節を生成するためのスクリプトを開く
    if(.not.present(config).or. index(config,".json")==0 )then
        ! デフォルトの設定を生成
        print *, "New fert-configuration >> fertconfig.json"
        call fertconf%open("fertconfig.json")
        write(fertconf%fh,*) '{'
        write(fertconf%fh,*) '   "type": "fertilizer",'
            

        ! percentage
        write(fertconf%fh,*)  ' "total_kg": 0.10d0 '
        write(fertconf%fh,*)  ' "N_pct": 10.0d0 '
        write(fertconf%fh,*)  ' "P_pct": 10.0d0 '
        write(fertconf%fh,*)  ' "K_pct": 10.0d0 '
        write(fertconf%fh,*)  ' "Ca_pct": 0.0d0    '
        write(fertconf%fh,*)  ' "Mg_pct": 0.0d0    '
        write(fertconf%fh,*)  ' "S_pct":  0.0d0 '
        write(fertconf%fh,*)  ' "Fe_pct": 0.0d0    '
        write(fertconf%fh,*)  ' "Mn_pct": 0.0d0    '
        write(fertconf%fh,*)  ' "B_pct":  0.0d0 '
        write(fertconf%fh,*)  ' "Zn_pct": 0.0d0    '
        write(fertconf%fh,*)  ' "Mo_pct": 0.0d0    '
        write(fertconf%fh,*)  ' "Cu_pct": 0.0d0    '
        write(fertconf%fh,*)  ' "Cl_pct": 0.0d0    '
        write(fertconf%fh,*) '}'
        conf="fertconfig.json"
        call fertconf%close()
    else
        conf = trim(config)
    endif
    
    call fertconf%open(trim(conf))
    blcount=0
    do
        read(fertconf%fh,'(a)') line
        print *, trim(line)
        if( adjustl(trim(line))=="{" )then
            blcount=1
            cycle
        endif
        if( adjustl(trim(line))=="}" )then
            exit
        endif
        
        if(blcount==1)then
            
            if(index(line,"type")/=0 .and. index(line,"fertilizer")==0 )then
                print *, "ERROR: This config-file is not for soybean"
                return
            endif


            if(index(line,"total_kg")/=0 )then
                ! 生育ステージ
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%total_kg
            endif


            if(index(line,"N_pct")/=0)then
                rmc = index(line,",")
                if(rmc /=0 )then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),* ) obj%N_pct
            endif

            if(index(line,"P_pct")/=0)then
                rmc = index(line,",")
                if(rmc /=0 )then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),* ) obj%P_pct
            endif

            if(index(line,"K_pct")/=0)then
                rmc = index(line,",")
                if(rmc /=0 )then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),* ) obj%K_pct
            endif

            if(index(line,"Ca_pct")/=0)then
                rmc = index(line,",")
                if(rmc /=0 )then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),* ) obj%Ca_pct
            endif

            if(index(line,"Mg_pct")/=0)then
                rmc = index(line,",")
                if(rmc /=0 )then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),* ) obj%Mg_pct
            endif

            if(index(line,"S_pct")/=0)then
                rmc = index(line,",")
                if(rmc /=0 )then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),* ) obj%S_pct
            endif

            if(index(line,"Fe_pct")/=0)then
                rmc = index(line,",")
                if(rmc /=0 )then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),* ) obj%Fe_pct
            endif

            if(index(line,"Mn_pct")/=0)then
                rmc = index(line,",")
                if(rmc /=0 )then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),* ) obj%Mn_pct
            endif

            if(index(line,"B_pct")/=0)then
                rmc = index(line,",")
                if(rmc /=0 )then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),* ) obj%B_pct
            endif

            if(index(line,"Zn_pct")/=0)then
                rmc = index(line,",")
                if(rmc /=0 )then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),* ) obj%Zn_pct
            endif

            if(index(line,"Mo_pct")/=0)then
                rmc = index(line,",")
                if(rmc /=0 )then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),* ) obj%Mo_pct
            endif

            if(index(line,"Cu_pct")/=0)then
                rmc = index(line,",")
                if(rmc /=0 )then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),* ) obj%Cu_pct
            endif

            if(index(line,"Cl_pct")/=0)then
                rmc = index(line,",")
                if(rmc /=0 )then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),* ) obj%Cl_pct
            endif

            cycle

        endif

    enddo
    call fertconf%close()
    obj%N_kg  = obj%N_kg + obj%N_pct/100.0d0*obj%total_kg
    obj%P_kg  = obj%P_kg + obj%P_pct/100.0d0*obj%total_kg
    obj%K_kg  = obj%K_kg + obj%K_pct/100.0d0*obj%total_kg
    obj%Ca_kg  = obj%Ca_kg + obj%Ca_pct/100.0d0*obj%total_kg
    obj%Mg_kg  = obj%Mg_kg + obj%Mg_pct/100.0d0*obj%total_kg
    obj%S_kg  = obj%S_kg + obj%S_pct/100.0d0*obj%total_kg
    obj%Fe_kg  = obj%Fe_kg + obj%Fe_pct/100.0d0*obj%total_kg
    obj%Mn_kg  = obj%Mn_kg + obj%Mn_pct/100.0d0*obj%total_kg
    obj%B_kg  = obj%B_kg + obj%B_pct/100.0d0*obj%total_kg
    obj%Zn_kg  = obj%Zn_kg + obj%Zn_pct/100.0d0*obj%total_kg
    obj%Mo_kg  = obj%Mo_kg + obj%Mo_pct/100.0d0*obj%total_kg
    obj%Cu_kg  = obj%Cu_kg + obj%Cu_pct/100.0d0*obj%total_kg
    obj%Cl_kg  = obj%Cl_kg + obj%Cl_pct/100.0d0*obj%total_kg

end subroutine
! ######################################################



end module 