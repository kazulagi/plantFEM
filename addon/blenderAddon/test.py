
bl_info = {
    "name": "plantFEM (Seed)",
    "author": "Haruka Tomobe",
    "version": (1, 0),
    "blender": (2, 80, 0),
    "location": "View3D > Add > Mesh > plantFEM Object",
    "description": "Adds a new plantFEM Object",
    "warning": "",
    "wiki_url": "",
    "category": "Add Mesh",
}


import bpy
from bpy.types import Operator
from bpy.props import FloatVectorProperty
from bpy_extras.object_utils import AddObjectHelper, object_data_add
from mathutils import Vector


class SAMPLE21_OT_CreateICOSphere(bpy.types.Operator):
    bl_idname = "object.sample21_create_icosphere"
    bl_label = "ICO Sphere"
    bl_description = "Add ICO Sphere."
    bl_options = {'REGISTER' , 'UNDO'}
    
    def execute(self, context):
        bpy.ops.mesh.primitive_ico_sphere_add()
        print("Sample : Add ICO Sphere.")
        
        return {'FINISHED'}
    
class SAMPLE21_OT_CreateCube(bpy.types.Operator):
    bl_idname = "object.sample21_create_cube"
    bl_label = "Cube"
    bl_description = "Add Cube."
    bl_options = {'REGISTER' , 'UNDO'}
    
    def execute(self, context):
        bpy.ops.mesh.primitive_cube_add()
        print("Sample : Add Cube")
        
        return{'FINISHED'}

def menu_fn(self, context):
    self.layout.separator()
    self.layout.operator(SAMPLE21_OT_CreateICOSphere.bl_idname)
    self.layout.operator(SAMPLE21_OT_CreateCube.bl_idname)
    
classes = [
    SAMPLE21_OT_CreateICOSphere,
    SAMPLE21_OT_CreateCube,
]
    

def register():
    for c in classes:
        bpy.utils.register_class(c)
    bpy.types.VIEW3D_MT_mesh_add.append(menu_fn)
    print("クラスを二つ使用するサンプルアドオンが有効化されました。")

def unregister():
    bpy.types.VIEW3D_MT_mesh_add.remove(menu_fn)
    for c in classes:
        bpy.utils.unregister_class(c)
    print("クラスを二つ使用するサンプルアドオンが無効化されました。")

if __name__ == "__main__":
    register()
