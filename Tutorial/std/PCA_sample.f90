use IOClass
use PCAClass

implicit none

type(IO_) :: f
type(PCA_) :: PCA

! Import data
call PCA%load(&
    filename="testdata.txt",&
    num_feature=3,&
    columns_are_feature_names=true)
! RUN PCA
call PCA%standarize()
call PCA%run()


! Export Data
call f%open("Feat_1_2.txt","w")
call f%write(PCA%DataFrame )
call f%close()
call f%splot()


call f%open("PC_1_2.txt","w")
call f%write(PCA%principalComponent() )
call f%close()
call f%plot()
end
