program main
    use addon_example
    implicit none
    type(addon_example_) :: obj
    call obj%set(realVal=8.0d0, intVal=-100)
    call obj%show()
end program