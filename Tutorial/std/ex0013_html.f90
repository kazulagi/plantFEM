program main
    use plantFEM
    implicit none

    type(Webserver_) :: ws
    type(FEMDomain_) :: sphere
    
    ! http サーバーの立ち上げ
    call ws%init()
    
    ! メッシュの生成
    call sphere%create(meshtype='Sphere3D',x_num=10,y_num=10,z_num=10,x_len=10.0d0,y_len=10.0d0,z_len=10.0d0)
    
    ! index.html　へ節点座標を表示
    ws%array = sphere%mesh%NodCoord
    ws%arrayTitleX="Coordinate of nodes (x, y, z)"
    ws%arrayTitleY="NodeID"
    
    ! 更新
    call ws%update()
    
    
end program main