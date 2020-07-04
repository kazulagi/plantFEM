program main

    use DictionaryClass
    use TermClass
    use MPIClass
    use ContactMechanicsClass

    implicit none

    type(Dictionary_)       :: Infiles
    type(MPI_)              :: MPIData
    type(ContactMechanics_) :: Obj
    type(Term_)             :: term
    character * 200         :: name,name1,name2,name3,name4,ElemType,SolverName
    call MPIData%Start()
    call Infiles%Init(4)
    call term%Init()
    call Obj%Init()
    call MPIData%End()
        
end program 