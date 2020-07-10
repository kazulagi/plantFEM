program main
    use plantFEM 
	implicit none

    type(FEMDomain_)::domain
    type(DiffusionEq_)::DiffusionEq
    type(LinearSolver_)::LinearSolver

    ! 領域 を　読み込む（ファイル名）
    call domain%import(node=.true.,file="./Node.txt")
    call domain%import(element=.true.,file="./Element.txt")
    
    ! 一次元定常拡散方程式について、有限要素法で離散化した連立方程式を作り、ソルバーにセットする。
    !LinearSolver = DiffusionEq%GetEquations(domain)
    
    ! ソルバーを使って、連立方程式を解く。
    !call LinearSolver%solve(Solver="BiCGSTAB",CRS=.true.)
    
    ! 結果を表示する。
    !print *, LinearSolver%x(:)

end program main