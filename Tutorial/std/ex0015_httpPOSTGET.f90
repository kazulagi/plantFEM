program main
    use plantFEM
    implicit none

    type(http_) :: api

    api%url = "https://www.google.com/"
    api%token = "your_access_token"
    api%channel = "#general"
    api%body = "hogehoge"

    call api%get()


end program main