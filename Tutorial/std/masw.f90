! code for MASW (Park, 1999)
use SpectreAnalysisClass
implicit none

type(SpectreAnalysis_) :: spa
complex(real64),allocatable :: fk(:,:),fc(:,:)
real(real64),allocatable :: all_data(:,:),t(:),x(:),buf(:,:),fk_real(:,:),fc_real(:,:),c_axis(:),f_axis(:),k_axis(:)
type(IO_) :: f

! create wave by
! Tutorial/std/create_waves_MASW.f90

! 模擬波形
all_data = zeros(f%numLine("sample_wave_mult_channel_"+zfill(1,4)+".txt"),200)
do i_i=1,200
    all_data(:,i_i:i_i) = f%to_array(name="sample_wave_mult_channel_"+zfill(i_i,4)+".txt",header=0,column=[2])
enddo
buf = f%to_array(name="sample_wave_mult_channel_"+zfill(1,4)+".txt",header=0,column=[1])

t   = buf(:,1)

c_axis   = linspace([0.001d0,1000.0d0],1000)

print *, size(buf,1)
fc = spa%masw(&
    t=t,&
    x=all_data,&
    position=0.010d0*[(i_i,i_i=1,15)],&
    c_axis=c_axis)

fc_real = abs(fc)


f_axis   = linspace([0.0d0,1.0d0/(t(2)-t(1))/2.0d0],size(fc,1))
print *, size(f_axis),size(c_axis)
print *, shape(fc_real)
call f%open("dispersion_image.txt","w")
do i_i=1,size(fc_real,1)
    do j_j=1,size(fc_real,2),10
        write(f%fh,*) f_axis(i_i), c_axis(j_j), fc_real(i_i,j_j) 
    enddo
    write(f%fh,*) ""
enddo
call f%close()
! peak-picking

call f%open("dispersion_curve.txt","w")
do i_i=1,size(fc_real,1) ! Hz
    write(f%fh,*) f_axis(i_i), c_axis(maxvalID(fc_real(i_i,:)))
enddo
call f%close()


! F-K spectrum
k_axis = linspace([0.00010d0,100.0d0],1000)
fk = spa%fk(&
    t=t,&
    x=all_data,&
    position=0.010d0*[(i_i,i_i=1,15)],&
    k_axis=k_axis)
print *, size(f_axis),size(c_axis)

call f%open("f-k.txt","w")
do i_i=1,size(f_axis),10
    do j_j=1,size(k_axis),10
        write(f%fh,*) f_axis(i_i),k_axis(j_j), abs(fk(i_i,j_j))
    enddo
    write(f%fh,*) ""
enddo
call f%close()


end