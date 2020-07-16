program main
    use plantFEM
    implicit none

    ! 静的型付け
    type(Random_) :: random
    type(IO_) :: f
    real(real64),allocatable :: dat(:,:)
    real(real64),allocatable :: dat1(:,:)
    real(real64),allocatable :: dat2(:,:)

    ! 乱数入り配列の作成
    dat = random%randn(3,5) 

    ! 配列の表示
    call showArray(dat)

    ! 計算＋配列の表示
    call showArray(dat*10.0d0)
    call showArray(dat+2.0d0*dat)

    ! 配列サイズの表示
    call showArraySize(dat) 

    ! 配列の書き出し
    call savetxt(dat,"./","array1",".txt")
    
    ! 配列の読み出し
    dat1 = loadtxt("./","array1",".txt")
    
    
    ! 配列の表示
    call showArray(dat1)
    

    return    
    ! または、たくさん配列を並べるときにはこれが便利
    call f%open("./","array2",".txt")
    call writeArray(f%fh, dat)
    call f%close()

    
    ! 読む時
    call f%open("./","array2",".txt")
    call loadArray(f%fh, dat2)
    call f%close()

    call showArray(dat1)
    

end program 