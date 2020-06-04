import bpy
import os

def write_some_data(context, filepath, use_some_setting):
    print("running write_sicrof_data...")
    f = open(filepath, 'w', encoding='utf-8')
    num_wa = 0
    #objs = [obj for obj in scene.objects if obj.name.startswith("Seed_")]

    #f.write(str(objs) )
    
    
    sname = filepath
    sname = sname+".f90"
    sf = open(str(sname), 'w')
    sf.write("program main\n")
    sf.write("  use SeedClass\n")
    sf.write("  implicit none\n")


    # detect seed objects
    for i in range(len(bpy.context.editable_objects)):
        if str(bpy.context.editable_objects[i].data ).find('Mesh("') == -1:
            continue
            
        st = str(bpy.context.editable_objects[i].name )
        st = st.replace("bpy_struct,", " ")
        st = st.replace('Mesh("', " ")
        st = st.replace('")>', " ")
        st = st.replace('<', " ")
        st = st.replace(' ', "")
        st = st.replace('_RNA_UI',"")
        st = st.replace('_RNA_UI',"")
        
        # import the object is Seed_ object to script 
        if str(st).find("Seed_") >= 0 :
            sf.write("type(Seed_) :: seed"+str(i)+"\n")
            



    # detect seed objects
    for i in range(len(bpy.context.editable_objects)):
        #f.write( str(bpy.context.editable_objects[i].display_bounds_type ))
        #f.write("\n")
        if str(bpy.context.editable_objects[i].data ).find('Mesh("') == -1:
            continue
            
        #f.write("\n")
        #f.write( str(bpy.context.editable_objects[i].name ))

        f.write("\n")
        st = str(bpy.context.editable_objects[i].name )
        st = st.replace("bpy_struct,", " ")
        st = st.replace('Mesh("', " ")
        st = st.replace('")>', " ")
        st = st.replace('<', " ")
        st = st.replace(' ', "")
        st = st.replace('_RNA_UI',"")
        st = st.replace('_RNA_UI',"")
        f.write(st)
        f.write("\n")

        # import the object is Seed_ object to script 
        if str(st).find("Seed_") >= 0 :
            sf.write("call seed"+str(i)+"%create(Name='seed"+str(i)+"'")
            st = str(bpy.context.editable_objects[i].data )
            st = st.replace("bpy_struct,", " ")
            st = st.replace('Mesh("', " ")
            st = st.replace('")>', " ")
            st = st.replace('<', " ")
            st = st.replace(' ', "")
            st = st.replace('_RNA_UI',"")
            f.write(st)
            f.write("\n")
            if str(st).lower() == "sphere":
                sf.write(",MeshType='Sphere3D'")
            elif str(st).lower() == "cube":
                sf.write(",MeshType='Cube'")
            n=0
            m=0
            f.write("\n")
            for mykey, myvalue in bpy.context.editable_objects[i].items():
                n=n+1
                m=1
            f.write(str(n-m)+"\n")

            n=0
            m=0
            for mykey, myvalue in bpy.context.editable_objects[i].items():
                n=n+1
                if n == 1:
                    continue
                f.write( str(mykey) + " " + str(myvalue) )
                if str(mykey).lower() == "x_num" or str(mykey).lower() == "xnum" :
                    sf.write(",x_num="+str(myvalue) )
                if str(mykey).lower() == "y_num" or str(mykey).lower() == "ynum" :
                    sf.write(",y_num="+str(myvalue) )
                if str(mykey).lower() == "z_num" or str(mykey).lower() == "znum" :
                    sf.write(",z_num="+str(myvalue) )
                #if str(mykey) == "WaterAbsorption" :
                #    m=1
                #elif str(mykey) == "waterabsorption" :
                #    m=1
                #elif str(mykey) == "Waterabsorption" :
                #    m=1
                #elif str(mykey) == "waterAbsorption" :
                #    m=1
                #else:    
            f.write("\n")
            #num_wa=num_wa+m
            #f.write( str(bpy.context.editable_objects[i].active_material ))

            f.write("scale\n")
            for j in range(len(bpy.context.editable_objects[i].scale)):
                f.write( str(bpy.context.editable_objects[i].scale[j]) )
                f.write("\n")
                if j == 0:
                    sf.write(",x_len="+str( float(2.0)*float(bpy.context.editable_objects[i].scale[j]))+"0d0&\n")

                if j == 1:
                    sf.write(",y_len="+str( float(2.0)*float(bpy.context.editable_objects[i].scale[j]))+"0d0&\n")

                if j == 2:
                    sf.write(",z_len="+str( float(2.0)*float(bpy.context.editable_objects[i].scale[j]))+"0d0)\n")

            sf.write("\n")
            f.write("\n")
            f.write("location\n")
            f.write("\n")
            sf.write("call seed"+str(i)+"%move(")
            for j in range(len(bpy.context.editable_objects[i].location)):
                f.write( str(bpy.context.editable_objects[i].location[j]) )
                
                if j == 0:
                    sf.write("x="+str( float(bpy.context.editable_objects[i].location[j]))+"0d0&\n")

                if j == 1:
                    sf.write(",y="+str( float(bpy.context.editable_objects[i].location[j]))+"0d0&\n")

                if j == 2:
                    sf.write(",z="+str( float(bpy.context.editable_objects[i].location[j]))+"0d0)\n")
            sf.write("\n")
            f.write("\n")

            f.write("rotation\n")
            sf.write("call seed"+str(i)+"%rotate(")
            for j in range(len(bpy.context.editable_objects[i].delta_rotation_euler)):
                f.write( str(bpy.context.editable_objects[i].delta_rotation_euler[j]) )
                f.write("\n")
                
                if j == 0:
                    sf.write("x="+str( float(bpy.context.editable_objects[i].delta_rotation_euler[j]))+"0d0&\n")

                if j == 1:
                    sf.write(",y="+str( float(bpy.context.editable_objects[i].delta_rotation_euler[j]))+"0d0&\n")

                if j == 2:
                    sf.write(",z="+str( float(bpy.context.editable_objects[i].delta_rotation_euler[j]))+"0d0)\n")
            sf.write("\n")
            f.write("\n")
            sf.write("call seed"+str(i)+"%gmsh(Name='seed"+str(i)+"')")
            sf.write("\n")

    #for i in range(len(bpy.context.object.location)):
    #    f.write(str(bpy.context.object.location[i]))
    #    f.write("\n")
    #f.write("Hello World %s" % use_some_setting)
    f.write("EOF")
    f.close()

    f = open(filepath, 'r', encoding='utf-8')
    
    f.close()


    sf.write("end program")
    sf.close()
    #create .f90 script
    


    return {'FINISHED'}


# ExportHelper is a helper class, defines filename and
# invoke() function which calls the file selector.
from bpy_extras.io_utils import ExportHelper
from bpy.props import StringProperty, BoolProperty, EnumProperty
from bpy.types import Operator


class ExportSomeData(Operator, ExportHelper):
    """This appears in the tooltip of the operator and in the generated docs"""
    bl_idname = "export_test.some_data"  # important since its how bpy.ops.import_test.some_data is constructed
    bl_label = "Export SiCroF Data"

    # ExportHelper mixin class uses this
    filename_ext = ".sicrof"

    filter_glob: StringProperty(
        default="*.sicrof",
        options={'HIDDEN'},
        maxlen=255,  # Max internal buffer length, longer would be clamped.
    )

    # List of operator properties, the attributes will be assigned
    # to the class instance from the operator settings before calling.
    use_setting: BoolProperty(
        name="Example Boolean",
        description="Example Tooltip",
        default=True,
    )

    type: EnumProperty(
        name="Example Enum",
        description="Choose between two items",
        items=(
            ('OPT_A', "First Option", "Description one"),
            ('OPT_B', "Second Option", "Description two"),
        ),
        default='OPT_A',
    )

    def execute(self, context):
        return write_some_data(context, self.filepath, self.use_setting)


# Only needed if you want to add into a dynamic menu
def menu_func_export(self, context):
    self.layout.operator(ExportSomeData.bl_idname, text="Export SiCroF")


def register():
    bpy.utils.register_class(ExportSomeData)
    bpy.types.TOPBAR_MT_file_export.append(menu_func_export)


def unregister():
    bpy.utils.unregister_class(ExportSomeData)
    bpy.types.TOPBAR_MT_file_export.remove(menu_func_export)


if __name__ == "__main__":
    register()

    # test call
    bpy.ops.export_test.some_data('INVOKE_DEFAULT')
