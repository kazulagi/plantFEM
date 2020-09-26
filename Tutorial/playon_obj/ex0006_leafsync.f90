program main
    use plantFEM
    implicit none


    type(Soybean_) :: soy
    type(Light_)   :: sun
    type(Air_)     :: air
    type(IO_)      :: f
    integer(int32) :: i,j

    ! 太陽を生成, 設定はデフォルト
    call sun%init()
    sun%maxppfd = 1400.0d0

    ! 大気を生成, 設定はデフォルト
    call air%init()

    ! ダイズを生成, 設定はデフォルト
    call soy%init("soyconfig.json")
    call soy%leaf(1)%rotate(y=radian(90),z=radian(45) )
    call soy%leaf(2)%rotate(y=radian(90),z=radian(-45) )

    ! ダイズをCO2を低下させつつ8時間×5日間成長させる
    call f%Open("A-Ci_curve.txt")
    air%CO2 = 380.0d0
    call soy%grow(dt = 60.0d0*60.0d0*8.0d0,light=sun, air=air)
    do i=1,size(soy%leaf(1)%A)
        write(f%fh,*) air%CO2, soy%leaf(1)%A(i)
    enddo

    air%CO2 = 300.0d0
    call soy%grow(dt = 60.0d0*60.0d0*8.0d0,light=sun, air=air)
    do i=1,size(soy%leaf(1)%A)
        write(f%fh,*) air%CO2, soy%leaf(1)%A(i)
    enddo

    air%CO2 = 250.0d0
    call soy%grow(dt = 60.0d0*60.0d0*8.0d0,light=sun, air=air)
    do i=1,size(soy%leaf(1)%A)
        write(f%fh,*) air%CO2, soy%leaf(1)%A(i)
    enddo

    air%CO2 = 200.0d0
    call soy%grow(dt = 60.0d0*60.0d0*8.0d0,light=sun, air=air)
    do i=1,size(soy%leaf(1)%A)
        write(f%fh,*) air%CO2, soy%leaf(1)%A(i)
    enddo

    air%CO2 = 150.0d0
    call soy%grow(dt = 60.0d0*60.0d0*8.0d0,light=sun, air=air)
    do i=1,size(soy%leaf(1)%A)
        write(f%fh,*) air%CO2, soy%leaf(1)%A(i)
    enddo

    air%CO2 = 100.0d0
    call soy%grow(dt = 60.0d0*60.0d0*8.0d0,light=sun, air=air)
    do i=1,size(soy%leaf(1)%A)
        write(f%fh,*) air%CO2, soy%leaf(1)%A(i)
    enddo


    air%CO2 = 50.0d0
    call soy%grow(dt = 60.0d0*60.0d0*8.0d0,light=sun, air=air)
    do i=1,size(soy%leaf(1)%A)
        write(f%fh,*) air%CO2, soy%leaf(1)%A(i)
    enddo
    
    call f%close()

    call soy%gmsh("test")


    ! 出力
    call f%open("source.txt")
    do i=1,size(soy%leaf)
        if(soy%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            do j=1,size(soy%leaf(i)%source)
                write(f%fh,*) soy%leaf(i)%source(j)
            enddo
            print *, sum(soy%leaf(i)%source)
        endif
        write(f%fh,*) " "
    enddo
    call f%close()
    

    
end program main