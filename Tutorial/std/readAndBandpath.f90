use IOClass
use SpectreAnalysisClass
implicit none

type(SpectreAnalysis_) :: speana
real(real64),allocatable :: z(:),z_b(:), y(:), x(:), t(:)
type(IO_) :: f

real(real64),allocatable :: freq(:), PowerSpectrum(:)
integer(int32) :: n = 100000

call speana%init(&
    sampling_Hz = 100.0   & ! 10 Hz
)

!call f%open("../500m_50cm_light_PCBiCG/Array_A_0037_dim_3.txt")
call f%open("x.txt")
call f%read(t,z)
call f%close()
call f%plot(option="with lines")

z_b = speana%bandpath(x=z,freq_range=[0.50,0.60])

call f%open("z_bandpath.txt","w")
call f%write(t,z_b)
call f%close()
call f%plot(option="with lines")

end