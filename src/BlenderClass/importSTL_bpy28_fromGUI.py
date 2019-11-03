bl_info = {
    "name": "STL Object",
    "author": "Haruka Tomobe",
    "version": (1, 0),
    "blender": (2, 80, 0),
    "location": "View3D > Add > Mesh > STL Object",
    "description": "Adds a new STL Object",
    "warning": "",
    "wiki_url": "",
    "category": "Add Mesh",
}



def read_some_data(context, filepath, use_some_setting,outfilepath):
    print("running read_some_data...")
    #f = open(filepath, 'r', encoding='utf-8')
    #data = f.read()
    #f.close()
    #outfilepath=filepath
    
    print("1")
    fout = open("filepath.txt", 'w', encoding='utf-8')
    fout.write(filepath)
    fout.close()


    # would normally load the data here
    # print(data)
    


    return {'FINISHED'}

import bpy
import os
import sys
from bpy.types import Operator
from bpy.props import FloatVectorProperty
from bpy_extras.object_utils import AddObjectHelper, object_data_add
from mathutils import Vector
from bpy_extras.io_utils import ImportHelper
from bpy.props import StringProperty, BoolProperty, EnumProperty





class ImportSomeData(Operator, ImportHelper):
    """This appears in the tooltip of the operator and in the generated docs"""
    bl_idname = "import_test.some_data"  # important since its how bpy.ops.import_test.some_data is constructed
    bl_label = "Import Some Data"
    outfilepath = ""


    print("2")
    # ImportHelper mixin class uses this
    filename_ext = ".stl"

    filter_glob: StringProperty(
        default="*.stl",
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

        print("3")
        outfilepath=""
        read_some_data(context, self.filepath, self.use_setting, outfilepath)
        print("Path="+self.filepath)
        f=open("filepath.txt",'w')
        f.write(self.filepath)
        f.close()
        return {'FINISHED'}
    

    
        









def add_object(self, context):
    scale_x = self.scale.x
    scale_y = self.scale.y

    verts = [
        Vector((-1 * scale_x, 1 * scale_y, 0)),
        Vector((1 * scale_x, 1 * scale_y, 0)),
        Vector((1 * scale_x, -1 * scale_y, 0)),
        Vector((-1 * scale_x, -1 * scale_y, 0)),
    ]

    edges = []
    faces = [[0, 1, 2, 3]]

    mesh = bpy.data.meshes.new(name="New Object Mesh")
    mesh.from_pydata(verts, edges, faces)
    # useful for development when the mesh may be invalid.
    # mesh.validate(verbose=True)

    print("4")
    object_data_add(context, mesh, operator=self)


class OBJECT_OT_add_object(Operator, AddObjectHelper):
    """Create a new Mesh Object"""
    bl_idname = "mesh.add_object"
    bl_label = "Add STL Object"
    bl_options = {'REGISTER', 'UNDO'}
    outfilepath=""
    scale: FloatVectorProperty(
        name="scale",
        default=(1.0, 1.0, 1.0),
        subtype='TRANSLATION',
        description="scaling",
    )

    def execute(self, context):
        bpy.ops.import_test.some_data('INVOKE_DEFAULT')

        print("5")
        f=open("filepath.txt","r")
        filepath=f.read()
        f.close()
        print("Hello, the path ="+filepath)
        bpy.ops.import_mesh.stl(filepath=filepath)
        #bpy.ops.import_mesh.stl(filepath="/home/haruka/test/seed.stl")
        #bpy.ops.import_mesh.stl(filepath="/home/haruka/test/test.stl")
        #add_object(self, context)

        return {'FINISHED'}


# Registration

def add_object_button(self, context):
    self.layout.operator(
        OBJECT_OT_add_object.bl_idname,
        text="Add STL-Object",
        icon='PLUGIN')


# This allows you to right click on a button and link to documentation
def add_object_manual_map():
    url_manual_prefix = "https://github.com/kazulagi/SiCroF"
    url_manual_mapping = (
        ("bpy.ops.mesh.add_object", "scene_layout/object/types.html"),
    )
    return url_manual_prefix, url_manual_mapping


# Only needed if you want to add into a dynamic menu
def menu_func_import(self, context):
    self.layout.operator(ImportSomeData.bl_idname, text="STL Import Operator")



def register():
    #bpy.utils.register_class(ImportSomeData)
    #bpy.types.TOPBAR_MT_file_import.append(menu_func_import)
    bpy.utils.register_class(OBJECT_OT_add_object)
    bpy.utils.register_manual_map(add_object_manual_map)
    bpy.types.VIEW3D_MT_mesh_add.append(add_object_button)


def unregister():
    #bpy.utils.unregister_class(ImportSomeData)
    #bpy.types.TOPBAR_MT_file_import.remove(menu_func_import)
    bpy.utils.unregister_class(OBJECT_OT_add_object)
    bpy.utils.unregister_manual_map(add_object_manual_map)
    bpy.types.VIEW3D_MT_mesh_add.remove(add_object_button)




if __name__ == "__main__":
    
    print("0")
    register()

    print("6")
    #bpy.ops.import_test.some_data('INVOKE_DEFAULT')

