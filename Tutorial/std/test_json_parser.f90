program main
    use IOClass
    implicit none
    
    type(IO_) :: f


    print *, f%parse_json("my_nested_data.json",&
        to_list("test_1","test_2","value"))
    print *, f%parse_json("my_nested_data.json",&
        to_list("test_1","test_2","test_3","test_4","test_5","test_6"))
    
end program main