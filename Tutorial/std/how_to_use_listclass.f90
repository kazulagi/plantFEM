program main
    use ListClass
    implicit none
    
    type(List_) :: list,lists(1:3)
    
    ! examples#1 get and append
    list = to_list("hello","world")
    print *, "<<< Ex. #1-1 >>> "
    print *, list%get(1)
    print *, list%get(2)

    call list%append("!")
    print *, "<<< Ex. #1-2 >>> "
    print *, list%get(3)

    ! example#2 repeat
    list = to_list("NS","EW","UD")
    print *, "<<< Ex. #2-1 >>> "
    call list%print()

    print *, "<<< Ex. #2-2 >>> "
    list%content(:) = list%content(:) // list%content(:) // list%content(:)
    call list%print()

    print *, "<<< Ex. #2-3 >>> "
    list = list // list
    call list%print()

    ! example #3 convert 
    print *, "<<< Ex. #3-1 >>> "
    list = to_list([1,2,3])
    call list%print()

    print *, "<<< Ex. #3-2 >>> "
    list = to_list([1.0,2.0,300000000.0])
    call list%print()


    print *, "<<< Ex. #3-3 >>> "
    list = to_list([1.0d0,2.0d0,3.0d0])
    call list%print()

    ! example #4 joint different types
    print *, "<<< Ex. #4-1 >>> "
    lists(1) = to_list("NS (","EW (","UD (") 
    lists(2) = to_list([1,2,3]) 
    lists(3) = to_list("ch)",num_repeat=3)
    list%content(:) = lists(1)%content(:) // lists(2)%content(:)  // lists(3)%content(:)
    call list%print()

    print *, "<<< Ex. #4-2 >>> "
    lists(1) = to_list("NS (","EW (","UD (") 
    lists(2) = to_list([1,2,3]) 
    lists(3) = to_list("ch) ",num_repeat=3)
    list%content(:) = lists(1)%content(:) // lists(2)%content(:)  // lists(3)%content(:)
    call list%print()
    

end program main