import bpy


def write_some_data(context, filepath, use_some_setting):
    print("running write_sicrof_data...")
    f = open(filepath, 'w', encoding='utf-8')
    for i in range(len(bpy.context.editable_objects)):
        #f.write( str(bpy.context.editable_objects[i].display_bounds_type ))
        #f.write("\n")
        f.write( str(bpy.context.editable_objects[i].data ))
        f.write("\n")
        n=0
        m=0
        f.write("\n")
        for mykey, myvalue in bpy.context.editable_objects[i].items():
            n=n+1
            m=1
        f.write(str(n-m)+"\n")
        for mykey, myvalue in bpy.context.editable_objects[i].items():
            f.write( str(mykey) + " " + str(myvalue) )
            f.write("\n")
            
        f.write("\n")
        f.write( str(bpy.context.editable_objects[i].name ))
        f.write("\n")
        f.write( str(bpy.context.editable_objects[i].active_material ))
        f.write("\n")
        f.write("location\n")
        for j in range(len(bpy.context.editable_objects[i].location)):
            f.write( str(bpy.context.editable_objects[i].location[j]) )
            f.write("\n")
        f.write("\n")
        f.write("rotation\n")
        for j in range(len(bpy.context.editable_objects[i].delta_rotation_euler)):
            f.write( str(bpy.context.editable_objects[i].delta_rotation_euler[j]) )
            f.write("\n")
        f.write("\n")
        
        f.write("scale\n")
        for j in range(len(bpy.context.editable_objects[i].scale)):
            f.write( str(bpy.context.editable_objects[i].scale[j]) )
            f.write("\n")
        f.write("\n")
    #for i in range(len(bpy.context.object.location)):
    #    f.write(str(bpy.context.object.location[i]))
    #    f.write("\n")
    #f.write("Hello World %s" % use_some_setting)
    f.close()

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
