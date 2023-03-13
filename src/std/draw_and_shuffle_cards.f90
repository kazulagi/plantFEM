program main
    use RandomClass
    implicit none

    type(Random_) :: random
    integer(int32),allocatable :: cards(:)
    integer(int32) :: i

    cards = [(i,i=1,13)]

    ! draw one
    print *, "Draw one; "
    do i=1,13
        print *, random%drawOne(cards)
    enddo
    print *, "Draw three; "
    
    ! draw three
    cards = [(i,i=1,13)]
    print *, random%draw(cards,num=3)
    print *, cards

    ! shuffle
    cards = [(i,i=1,13)]
    call random%shuffle(cards)
    print *, cards
    

end program main