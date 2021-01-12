import bpy
import os
import sys

def write_some_data(context, filepath, use_some_setting):
    print("running script...")
    scriptname = str(filepath)

    fn = str(filepath)
    fn =str(os.path.dirname(fn))
    
    os.chdir(fn)
    sc = open("bl.sh", 'w')
    sc.write("python3 plantfem.py -s "+str(scriptname))
    sc.close()
    #os.system("python3 plantFEM.py -s install")
    os.system("sh bl.sh")
    #scriptname

    #ret = os.system("python3 plantFEM.py -s "+scriptname)
    #messagebox.showinfo("plantFEM", "plantFEM is installed!"+str (ret))
    return {'FINISHED'}    

# ExportHelper is a helper class, defines filename and
# invoke() function which calls the file selector.
from bpy_extras.io_utils import ExportHelper
from bpy.props import StringProperty, BoolProperty, EnumProperty
from bpy.types import Operator


class ExportSomeData(Operator, ExportHelper):
    """This appears in the tooltip of the operator and in the generated docs"""
    bl_idname = "export_test.some_data"  # important since its how bpy.ops.import_test.some_data is constructed
    bl_label = "Run plantFEM (.f90)"

    # ExportHelper mixin class uses this
    filename_ext = ".f90"

    filter_glob: StringProperty(
        default="*.f90",
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
    self.layout.operator(ExportSomeData.bl_idname, text="run plantFEM")


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

    #messagebox.askquestion("Run this simulation?", "Run this simulation?")
