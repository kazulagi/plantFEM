use plantfem

implicit none

type(Preprocessing_) :: root,soil
type(FEMDomain_) :: domain
type(MPI_) :: mpid
character(10) :: name 
character(10) :: project = "soil"
character(20) :: ElemType = "LinearRectangularGp4"
character(10) :: str_id = "1"

call mpid%start()

! in this picture,
! root color is (R:0, G:255,B:255)
! and soil color is (R:0, G:0,B:0)
name = "test.png"


! Get RGB value of root
call root%ImportPictureName(name)
call root%GetPixcelSize(mpid)
call root%SetColor(0,255,255)
call root%GetPixcelByRGB(mpid,err=5,onlycoord=.true.)

! Get Outline of root
call root%GetSurfaceNode(mpid)
call root%AssembleSurfaceElement(mpid,dim=2,threshold=5,DelRange=5)

!<<< CAUTION! please check GetSurface_pid_***.txt>>>

!<<< CAUTION! If you want to use 4-node element, gmsh>tool>option>mesh>general
!    then check "recombine elements", and set option as default.>>>
! Convert SurfaceNod to .geo
call root%ExportGeoFile(mpid,Name=trim(project)//"mesh"//trim(str_id)//".geo" )
        
! Run Gmsh to convert .geo to .msh
call root%ConvertGeo2Msh(mpid, Name=trim(project)//"mesh"//trim(str_id)//".geo" )
call root%ConvertGeo2Inp(mpid, Name=trim(project)//"mesh"//trim(str_id)//".geo" )
call root%ConvertGeo2Mesh(mpid,Name=trim(project)//"mesh"//trim(str_id)//".geo" )


call root%ConvertMesh2Scf(mpid,ElementType=ElemType,&
     Name=trim(project)//"mesh"//trim(str_id)//".mesh" )
call root%FEMDomain%checkconnectivity(fix=.true.)


! create soil mesh

call soil%ImportPictureName(name)
call soil%GetPixcelSize(mpid)
call soil%SetColor(0,0,0)
call soil%GetPixcelByRGB(mpid,err=5,onlycoord=.true.)

! Get Outline (simple outline)
! see soil as a box
call soil%GetSurfaceNode(mpid,box=.true.)
call soil%modifySuefaceNode(Mesh=root%FEMDomain%Mesh,boolean="diff")

! Convert SurfaceNod to .geo
call soil%ExportGeoFile(mpid,Name=trim(project)//"soil"//trim(str_id)//".geo" )

! Run Gmsh to convert .geo to .msh
call soil%ConvertGeo2Msh(mpid ,Name=trim(project)//"soil"//trim(str_id)//".geo" )
call soil%ConvertGeo2Inp(mpid ,Name=trim(project)//"soil"//trim(str_id)//".geo" )
call soil%ConvertGeo2Mesh(mpid,Name=trim(project)//"soil"//trim(str_id)//".geo" )

call mpid%end()

end