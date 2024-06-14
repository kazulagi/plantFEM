program main
    use IOClass
    use ArrayClass
    implicit none
    
    type(IO_) :: f

    print *, f%parse_json("my_nested_data.json",&
        to_list("test_1","test_2","value"))

    print *, f%parse_json("my_nested_data.json",&
        to_list("test_1","test_2","test_3","test_4","test_5","test_6"))

    print *, f%parse_json("my_nested_data.json",&
        to_list("test_1","test_2","test_3","test_4","test_5","test_8"))

    print *, f%parse_json("my_nested_data.json",&
        to_list("test_1","test_2","test_3","test_4","test_5","test_9"))

    call print(to_array(f%parse_json("my_nested_data.json",&
        to_list("test_1","test_2","test_3","test_4","test_5","test_9"))))


end program main