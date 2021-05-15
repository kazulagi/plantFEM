subroutine getDiffusionEquation1D(domain,LinearSolver)
    use plantFEM
    implicit none

    type(FEMDomain_),intent(inout)::domain
    type(LinearSolver_)::LinearSolver
    integer(int32) :: i,j,n,elemnum,&
        nodeid_1,nodeid_2,elemid,numdirichlet,node_id,dboundid,materialid
    real(real64)   :: k,f,h_1,h_2,Le,x_1,x_2,val
    real(real64)   :: Kmat(2,2),fvec(2)

    elemnum = size(domain%mesh%elemnod,1)
    do i=1, elemnum
        elemid = i
        nodeid_1 = domain%mesh%elemnod(  elemid,1)
        nodeid_2 = domain%mesh%elemnod(  elemid,2)
        materialid = domain%mesh%elemmat(elemid)
        
        x_1 = domain%mesh%nodcoord(nodeid_1,1)
        x_2 = domain%mesh%nodcoord(nodeid_2,1)

        Le = abs(x_2 - x_1)
        k  = domain%materialprop%matpara(materialid,1)
        f  = domain%materialprop%matpara(materialid,2)

        Kmat(1,1)=  k/Le
        Kmat(1,2)= -k/Le
        Kmat(2,1)= -k/Le
        Kmat(2,2)=  k/Le

        fvec(1) = 0.50d0*f*Le
        fvec(2) = 0.50d0*f*Le

        call LinearSolver%set(nodeid_1,nodeid_1, entryvalue= Kmat(1,1) )
        call LinearSolver%set(nodeid_1,nodeid_2, entryvalue= Kmat(1,2) )
        call LinearSolver%set(nodeid_2,nodeid_1, entryvalue= Kmat(2,1) )
        call LinearSolver%set(nodeid_2,nodeid_2, entryvalue= Kmat(2,2) )

        call LinearSolver%set(nodeid_1, entryvalue=fvec(1) )
        call LinearSolver%set(nodeid_2, entryvalue=fvec(2) )

    enddo

    ! ディリクレ境界条件を導入する。
    numdirichlet = size(domain%Boundary%DboundNodID,1)
    
    do i=1,numdirichlet
        dboundid = i
        node_id = domain%Boundary%DboundNodID(dboundid,1)
        val     = domain%Boundary%DboundVal(dboundid,1)
        call LinearSolver%fix(node_id, entryvalue=val)
    enddo
end subroutine


program main
    use plantFEM 
    implicit none
    

    type(FEMDomain_)::domain ! 有限要素法シミュレーションのためのデータベースアプリ
    type(LinearSolver_)::LinearSolver ! 連立方程式を解くアプリ
    type(IO_) :: file ! ファイル編集アプリ 
    integer(int32) :: i !整数

    ! FEMデータベースアプリを起動して、データを読み込む

    ! 呼び出し方 >> 領域(domain)　を(%)　読み込む(import)（ファイル名）
    call domain%import(node=.true.,file="./Node.txt")
    call domain%import(element=.true.,file="./Element.txt")
    call domain%import(materialinfo=.true.,file="./Materialinfo.txt")
    call domain%import(Dirichlet=.true.,file="./DBoundary.txt")
    
    ! 一次元定常拡散方程式について、有限要素法で離散化した連立方程式を作り、ソルバーにセットする。
    ! （以下では、このアプリを自作します。）
    call getDiffusionEquation1D(domain,LinearSolver)
    
    ! ソルバーを使って、連立方程式を解く。
    ! 連立方程式を解くアプリ(LinearSolver) に(%) 連立方程式を解け(solve)、と命令する。（CRSBiCGSTABで）
    call LinearSolver%solve(Solver="BiCGSTAB",CRS=.true.)
    
    ! 結果を表示する。
    ! 連立方程式を解くアプリ(LinearSolver) の(%)　解(x)を表示する。
    print *, LinearSolver%x(:)

    ! 結果の書き出し
    call file%open("./"//"result2"//".txt")
    do i=1, size(LinearSolver%x)
        call file%write( str(LinearSolver%x(i)) )
    enddo
    call file%close()


end program main