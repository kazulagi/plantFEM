program main
    use DictionaryClass
    implicit none
    
    type(Dictionary_) :: dicts(4)
    
    call dicts(1)%update("key1","value1")
    call dicts(2)%update("key2",dicts(1))
    call dicts(2)%update("key4","hello")
    call dicts(2)%update("key5","good bye")
    call dicts(3)%update("key3",dicts(2))
    
    print *, to_dict(to_dict(dicts(3) > "key3") > "key2" ) > "key1"
    print *, to_dict(dicts(3) > "key3") > "key4"
    print *, to_dict(dicts(3) > "key3") > "key5"
    print *, to_dict(to_dict(dicts(3) > "key3") > "key6") > "key7"
    
end program main