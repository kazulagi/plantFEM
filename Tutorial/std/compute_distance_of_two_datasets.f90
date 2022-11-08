use ArrayClass
use IOClass
implicit none

type(Random_) :: random
real(real64),allocatable :: exact_solution(:,:), trial_solution(:,:),best_solution(:,:)
integer(int32) :: trial_id,id
real(real64) :: dist_val,best_dist
type(IO_) :: f,gp
character(:),allocatable :: filename

exact_solution = zeros(6,2)
exact_solution(:,1) = [-1.0, 2.0, 2.5, 3.0, 4.0, 5.0]
exact_solution(:,2) = [ 1.2, 1.8, 2.6, 1.9,-2.0, 3.0]

call gp%open("plot.gp","w")
call gp%write("unset key")

do trial_id = 1, 10000
    trial_solution= zeros(6,2)
    trial_solution(:,1) = [-1.0, 2.0, 2.5, 3.0, 4.0, 5.0]
    trial_solution(:,2) = [ 1.2, 1.8, 2.6, 1.9,-2.0, 3.0]*random%randn(6)*5.00d0
    
    ! compute distance
    dist_val = distance(exact_solution,trial_solution)

    filename = "distance_"+zfill(trial_id,5)+".txt"

    if(trial_id==1)then
        best_solution = trial_solution
        best_dist  = dist_val
        call gp%write("splot '"+filename+"' w l ")
    else
        if(best_dist > dist_val )then
            best_solution = trial_solution
            best_dist     = dist_val
        endif
        call gp%write("replot '"+filename+"' w l ")
    endif

    call f%open(filename,"w")
    do id = 1, size(trial_solution,1)
        write(f%fh,*) trial_solution(id,1:2),dist_val
    enddo
    call f%close()
enddo
call gp%close()

call print("exact_solution")
call f%open("exact.txt","w")
call f%write(exact_solution)
call f%close()
call print(exact_solution)


call print("best_solution")
call f%open("best.txt","w")
call f%write(best_solution)
call f%close()
call print(best_solution)
call print("best distance : "+  str(best_dist) )

end