program main
    use SoybeanClass
    implicit none

    type(Soybean_) :: soy
    
    ! let us deform soybean!!
    call soy%init(config="soy.json")
    !call soy%init(config="Tutorial/obj/realSoybeanConfig.json")
    
    ! (1) 設定事項の確認
    call soy%checkProperties(Simulation=PF_DEFORMATION_ANALYSIS)
    
    ! この状態では，ただ空のオブジェクトがあるだけ．
    ! (2)材料条件をどう設定する?
    
    ! 範囲，属性，ID指定できるようにする．
    ! 材料情報を設定する．
    call soy%setProperties(density=.true.)
    call soy%setProperties(YoungModulus=.true.,default_value=1.0d0)
    call soy%setProperties(PoissonRatio=.true.,default_value=0.2d0)
    ! 初期条件を設定する．
    call soy%setProperties(InitialDisplacement=.true.,default_value=0.0d0)
    call soy%setProperties(InitialStress=.true.,default_value=0.0d0)
    ! 境界条件を設定する．
    call soy%setProperties(BoundaryTractionForce=.true.,default_value=0.0d0)
    call soy%setProperties(BoundaryDisplacement=.true.,default_value=0.0d0)
    ! 重力加速度を設定する．
    call soy%setProperties(Gravity=.true.,default_value=0.0d0)

    ! 必要な情報が全て設定されているか確認する．
    call soy%checkProperties(Simulation=PF_DEFORMATION_ANALYSIS)

    ! 必要な情報が全て設定されていれば，戻り値は.true.
    print *, soy%readyFor(Simulation=PF_DEFORMATION_ANALYSIS)
    
    ! (6) シミュレーションの実行
    !call soy%DeformationAnalysis()

    ! (7) 結果の確認
    !call soy%vtk("soy_deformed/soy")

end program main