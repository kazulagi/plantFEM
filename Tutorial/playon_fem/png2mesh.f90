use plantfem

implicit none

type(Preprocessing_) :: plant
type(MPI_) :: mpid
character(100) :: name 
character(10) :: str_id = "1"
integer(int32) :: arg_id = 1

! MPIで並列化する用のやつです。特に設定していなければcpu-core=1です。
call mpid%start()

! コマンドライン引数で画像ファイル名をとってくる
call getarg(arg_id,name)

! Get RGB value of plant
call plant%ImportPictureName(name)
call plant%GetPixcelSize(mpid)
call plant%SetColor(0,255,255)

! R:0, G:255, B:255の部分だけをFEMメッシュ分割用にとってくる
call plant%GetPixcelByRGB(mpid,err=5,onlycoord=.true.)

! Get Outline of plant
call plant%GetSurfaceNode(mpid)
call plant%AssembleSurfaceElement(mpid,dim=2,threshold=5,DelRange=5)

!<<< CAUTION! please check GetSurface_pid_***.txt>>>

! 4節点要素の生成にはGmshのGUI上で事前に設定が必要です。
!<<< CAUTION! If you want to use 4-node element, gmsh>tool>option>mesh>general
!    then check "recombine elements", and set option as default.>>>
! Convert SurfaceNod to .geo

call plant%ExportGeoFile(mpid,Name="test.geo")
        
! Run Gmsh to convert .geo to .msh, inp, ..etc.
call plant%ConvertGeo2Msh(mpid, Name="test.geo")
call plant%ConvertGeo2Inp(mpid, Name="test.geo")
call plant%ConvertGeo2Mesh(mpid,Name="test.geo")
call plant%ConvertGeo2VTK(mpid,Name="test.geo")

! *.vtkなどとしてFEMメッシュが出力されました。

call mpid%end()

end