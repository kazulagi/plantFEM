program main
    use MathClass
    implicit none

    integer(int32) :: d, e, lambda,prime1,prime2,seed,id_rsa(2),id_rsa_pub(2)
    integer(int32) :: message, ciphertext,a,m

    prime1=61
    prime2=53
    seed=17
    call rsa_keygen(prime1,prime2,seed,id_rsa,id_rsa_pub)
    
    message=1234
    print *, "message = ", message
    
    ciphertext = rsa_encrypt(id_rsa_pub,message)
    print *, "ciphertext = ", ciphertext

    message = rsa_decrypt(id_rsa,ciphertext)
    print *, "message = ", message
    print *, ">>>>>>>>>>>>>>>>>>>>>>>>>"
    print *, "Private key ",id_rsa
    print *, "Public key  ",id_rsa_pub
    print *, "<<<<<<<<<<<<<<<<<<<<<<<<<"
    
end program main