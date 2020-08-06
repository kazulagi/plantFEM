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

    mesh = bpy.data.meshes.new(name="plantFEM seed object")
    mesh.from_pydata(verts, edges, faces)
    # useful for development when the mesh may be invalid.
    # mesh.validate(verbose=True)
    object_data_add(context, mesh, operator=self)
    

class OBJECT_OT_add_object_seed(Operator, AddObjectHelper):
    """plantFEM seed object"""
    bl_idname = "mesh.add_object"
    bl_label = "Seed (plantFEM)"
    bl_options = {'REGISTER', 'UNDO'}


    scale: FloatVectorProperty(
        name="scale",
        default=(1.0, 1.0, 1.0),
        subtype='TRANSLATION',
        description="scaling",
    )

    def execute(self, context):
        
        #add_object(self, context)
        bpy.ops.mesh.primitive_uv_sphere_add(enter_editmode=False, location=(0, 0, 0))
        for obj in bpy.context.selected_objects:
            obj.name = "Seed_"
            obj.data.name = "Seed_"
            obj["x_num"]=int(10)        
            obj["y_num"]=int(10)
            obj["z_num"]=int(10)
            obj["YoungsModulus"]=float(100)
            obj["PoissonRatio"]=float(0.3)
            obj["Permiability"]=float(0.0000001)
            obj["a_Psi"]=float(20)
            obj["a_P"]=float(40)
            obj["theta_eq"]=float(1)
            obj["Psi_eq"]=float(20)
            obj["a_E"]=float(0)
            obj["a_v"]=float(0)
            obj["E_eq"]=float(100)
            obj["v_eq"]=float(0.3)


        return {'FINISHED'}


class OBJECT_OT_add_object_soil(Operator, AddObjectHelper):
    """plantFEM seed object"""
    bl_idname = "mesh.add_object"
    bl_label = "Soil (plantFEM)"
    bl_options = {'REGISTER', 'UNDO'}


    scale: FloatVectorProperty(
        name="scale",
        default=(1.0, 1.0, 1.0),
        subtype='TRANSLATION',
        description="scaling",
    )

    def execute(self, context):
        
        #add_object(self, context)
        bpy.ops.mesh.primitive_cube_add(enter_editmode=False, location=(0, 0, 0))
        for obj in bpy.context.selected_objects:
            obj.name = "Soil_"
            obj.data.name = "Soil_"
            obj["x_num"]=int(10)        
            obj["y_num"]=int(10)
            obj["z_num"]=int(10)
            obj["YoungsModulus"]=float(100)
            obj["PoissonRatio"]=float(0.3)
            obj["Permiability"]=float(0.0000001)
            obj["a_Psi"]=float(20)
            obj["a_P"]=float(40)
            obj["theta_eq"]=float(1)
            obj["Psi_eq"]=float(20)
            obj["a_E"]=float(0)
            obj["a_v"]=float(0)
            obj["E_eq"]=float(100)
            obj["v_eq"]=float(0.3)


        return {'FINISHED'}

def menu_fn(self, context):
    self.layout.separator()
    self.layout.operator(OBJECT_OT_add_object_seed.bl_idname)
    self.layout.operator(OBJECT_OT_add_object_soil.bl_idname)
    
# Registration

def add_object_button_seed(self, context):
    self.layout.operator(
        OBJECT_OT_add_object_seed.bl_idname,
        text="Seed",
        icon='PLUGIN')



def add_object_button_soil(self, context):
    self.layout.operator(
        OBJECT_OT_add_object_soil.bl_idname,
        text="Soil",
        icon='PLUGIN')

# This allows you to right click on a button and link to documentation
def add_object_manual_map():
    url_manual_prefix = "https://github.com/kazulagi/plantFEM"
    url_manual_mapping = (
        ("bpy.ops.mesh.add_object", "README.md"),
    )
    return url_manual_prefix, url_manual_mapping

classes = [
    OBJECT_OT_add_object_seed,
    OBJECT_OT_add_object_soil,
]

def register():
    for c in classes:
        bpy.utils.register_class(c)
    #bpy.utils.register_class(OBJECT_OT_add_object_seed)
    #bpy.utils.register_class(OBJECT_OT_add_object_soil)
    #bpy.types.VIEW3D_MT_mesh_add.append(add_object_button_seed)
    #bpy.types.VIEW3D_MT_mesh_add.append(add_object_button_soil)
    bpy.types.VIEW3D_MT_mesh_add.append(menu_fn)


def unregister():
    bpy.types.VIEW3D_MT_mesh_add.remove(menu_fn)
    for c in classes:
        bpy.utils.unregister_class(c)

    #bpy.utils.register_class(OBJECT_OT_add_object_seed)
    #bpy.utils.register_class(OBJECT_OT_add_object_soil)
    #bpy.utils.unregister_manual_map(add_object_manual_map)
    #bpy.types.VIEW3D_MT_mesh_add.remove(add_object_button_seed)
    #bpy.types.VIEW3D_MT_mesh_add.remove(add_object_button_soil)


if __name__ == "__main__":
    register()
#    bpy.ops.mesh.primitive_uv_sphere_add(enter_editmode=False, location=(0, 0, 0))
#    for obj in bpy.context.selected_objects:
#        obj.name = "Seed_"
#        obj.data.name = "Seed_"
#        obj["x_num"]=int(10)        
#        obj["y_num"]=int(10)
#        obj["z_num"]=int(10)
#        obj["YoungsModulus"]=float(100)
#        obj["PoissonRatio"]=float(0.3)
#        obj["Permiability"]=float(0.0000001)
#        obj["a_Psi"]=float(20)
#        obj["a_P"]=float(40)
#        obj["theta_eq"]=float(1)
#        obj["Psi_eq"]=float(20)
#        obj["a_E"]=float(0)
#        obj["a_v"]=float(0)
#        obj["E_eq"]=float(100)
#        obj["v_eq"]=float(0.3)

