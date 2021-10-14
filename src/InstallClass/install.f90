program main
    use mpi

    integer :: i,n,ierr,my_rank,petot,fh
    character*17 :: FileName
    character*17 :: Filelist
    character*200 :: command
    
    call mpi_init(ierr)
    call mpi_comm_size(mpi_comm_world,petot,ierr)
    call mpi_comm_rank(mpi_comm_world,my_rank,ierr)
    fh=my_rank+10
    write (FileName, '(".install", i6.6, ".sh")') my_rank+1
    write (Filelist, '(".flist__", i6.6, ".sh")') my_rank+1
    ! create file list
    open(fh+100,file=".install_list",status="old")
    open(fh+200,file=Filelist,status="replace")   
    n=0
    do i=1,28
        n=n+1
        read(fh+100,'(A)') command
        if(i < 15 )then
            n=0
            write(fh+200,'(A)') command
            cycle
        endif

        if(my_rank+1==n)then
            n=0
            write(fh+200,'(A)') command
            cycle
        endif
    enddo
    close(fh+100)
    close(fh+200)
    call mpi_barrier(mpi_comm_world,ierr)

    ! create shell script
    open(fh,file=FileName)
    write(fh,'(A)') '#!/bin/sh -eu'

    !write(fh,'(A)') 'retry() {'
    !write(fh,'(A)') 'max_attempts=10'
    !write(fh,'(A)') 'cmd=10'
    !write(fh,'(A)') 'attempt_num=1'
    !write(fh,'(A)') 'until $cmd'
    !write(fh,'(A)') 'do'
    !write(fh,'(A)') '    if (( attempt_num == max_attempts ))'
    !write(fh,'(A)') '    then'
    !write(fh,'(A)') '        echo "Attempt $attempt_num failed and there are no more attempts left!"'
    !write(fh,'(A)') '        return 1'
    !write(fh,'(A)') '    else'
    !write(fh,'(A)') '        echo "Attempt $attempt_num failed! Trying again in $attempt_num seconds..."'
    !write(fh,'(A)') '        sleep 0'
    !write(fh,'(A)') '    fi'
    !write(fh,'(A)') 'done'
    !write(fh,'(A)') '}'

    write(fh,'(A)') "while read line"
    write(fh,'(A)') "do"
    write(fh,'(A)') 'echo "------------------------"'
    write(fh,'(A)') "echo $line"
    write(fh,'(A)') "$line"
    write(fh,'(A)') "done < "//Filelist
    close(fh)

    command="sh "//FileName
    print *, command
    call execute_command_line(command)
    call mpi_finalize(ierr)
end program