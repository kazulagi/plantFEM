program main
    use plantFEM
    implicit none

    type(FEMDomain_) :: dam
    real(real64) :: x(7),altitude(7),top
    integer(int32) :: i

    ! plantFEM用メッシュ生成スクリプト
    ! https://github.com/kazulagi/plantFEM

    ! メッシュの元となったデータ：
    ! 牧　隆泰, 1958, 日本水利施設進展の研究, 土木雑誌社
    ! p. 209 長谷池（岡山県児島郡鉾立村)

    ! 以下、メッシュ分割プログラム
    ! 要素分割数
    call dam%create(meshtype="Cube",x_num=30,y_num=30,z_num=12)
    ! 堤体寸法(上下流方向長,堤長, 堤高)
    !15:6.1=x:30.1
    call dam%resize(x=85.00d0,y=137.0d0,z=3.0d0)
    call dam%move(x=-5.0d0,z=-2.0d0)
    ! 上流からx(n) m 進んだ地点の高さがaltitude(n) m
    x(1) = 0.00d0; altitude(1) = 1.00d0;
    x(2) = 7.37d0; altitude(2) = 9.22d0;
    x(3) = 9.59d0; altitude(3) = 9.22d0;
    x(4) =19.67d0; altitude(4) =18.69d0;
    x(5) =38.48d0; altitude(5) =18.69d0;
    x(6) =74.01d0; altitude(6) = 8.60d0;
    x(7) =75.73d0; altitude(7) = 1.00d0;
    
    call dam%edit(x=x, altitude=altitude)

    ! 保存
    call dam%msh("長谷下池")
    call dam%stl("長谷下池")




end program main
