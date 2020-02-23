program main
    use MathClass
    implicit none

    integer(int32) :: d, e, lambda,prime1,prime2,seed,id_rsa(2),id_rsa_pub(2)
    integer(int32) :: message, ciphertext,a,m

    !e=4
    !lambda=11
    !d=inv_mod(e,lambda)
    !print *, d
    !debug inv_mod

    prime1=61
    prime2=53
    seed=17
    call rsa_keygen(prime1,prime2,seed,id_rsa,id_rsa_pub)
    
    message=1
    print *, "message = ", message
    
    ciphertext = rsa_encrypt(id_rsa_pub,message)
    print *, "ciphertext = ", ciphertext

    message = rsa_decrypt(id_rsa,ciphertext)
    print *, "message = ", message
print *, id_rsa
    
end program main