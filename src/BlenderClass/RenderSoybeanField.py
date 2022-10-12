import bpy
import numpy as np

def create_background():
    # remove initial cube
    bpy.data.objects["Cube"].select_set(True)
    bpy.ops.object.delete()

    # setup background
    bpy.ops.mesh.primitive_plane_add(size=2, enter_editmode=False, align='WORLD', location=(0, 0, 0), scale=(1, 1, 1))
    bpy.context.object.scale[0] = 100
    bpy.context.object.scale[1] = 100
    bpy.context.object.scale[2] = 1
    bpy.context.object.name = "Background"


    bpy.ops.material.new()
    bpy.data.objects['Background'].active_material=bpy.data.materials['Material.001']
    bpy.data.materials["Material.001"].node_tree.nodes["Principled BSDF"].inputs[0].default_value = (0.698192, 0.490812, 0.146267, 1)

def import_leaf_as_stl(filename):
    # import objects

    # leaf
    bpy.ops.import_mesh.stl(filepath=filename+".stl")

    #set color
    bpy.ops.material.new()
    bpy.data.objects[filename].active_material=bpy.data.materials['Material.002']
    bpy.data.materials["Material.002"].node_tree.nodes["Principled BSDF"].inputs[0].default_value = (0.0303391, 0.8, 0.0636039, 1)

def import_stem_as_stl(filename):
    # import objects

    #stem
    bpy.ops.import_mesh.stl(filepath=filename+".stl")
    #set color
    bpy.ops.material.new()
    bpy.data.objects[filename].active_material=bpy.data.materials['Material.003']
    bpy.data.materials["Material.003"].node_tree.nodes["Principled BSDF"].inputs[0].default_value = (0.549976, 0.8, 0.352695, 1)

def import_root_as_stl(filename):
    #root
    bpy.ops.import_mesh.stl(filepath=filename+".stl")

    #set color
    bpy.ops.material.new()
    bpy.data.objects[filename].active_material=bpy.data.materials['Material.004']
    bpy.data.materials["Material"].node_tree.nodes["Principled BSDF"].inputs[0].default_value = (0.769451, 0.8, 0.36234, 1)

def move_object_to_position(object_name, x, y, z):
    bpy.data.objects[object_name+"_leaf"].location[0] = x
    bpy.data.objects[object_name+"_leaf"].location[1] = y
    bpy.data.objects[object_name+"_leaf"].location[2] = z
    
    bpy.data.objects[object_name+"_root"].location[0] = x
    bpy.data.objects[object_name+"_root"].location[1] = y
    bpy.data.objects[object_name+"_root"].location[2] = z
    
    bpy.data.objects[object_name+"_stem"].location[0] = x
    bpy.data.objects[object_name+"_stem"].location[1] = y
    bpy.data.objects[object_name+"_stem"].location[2] = z

def set_camera_position(x, y, z):
    bpy.data.objects["Camera"].location[0] = x
    bpy.data.objects["Camera"].location[1] = y
    bpy.data.objects["Camera"].location[2] = z
    
def rotate_camera_around_object(angle):
    bpy.data.objects["Camera"].rotation_euler[0] = angle
    bpy.data.objects["Camera"].rotation_euler[1] = 0.0
    bpy.data.objects["Camera"].rotation_euler[2] = 0.0

if __name__ == "__main__":
    create_background()
    set_camera_position(0, 0, 3.0)
    rotate_camera_around_object(0.0)
    num_row = 10
    num_col = 10
    for i in range(num_row):
        for j in range(num_col):
            import_leaf_as_stl("soy_"+str(i)+"_"+str(j)+"_leaf")
            import_stem_as_stl("soy_"+str(i)+"_"+str(j)+"_stem")
            import_root_as_stl("soy_"+str(i)+"_"+str(j)+"_root")
    
    for i in range(1):
        #x = np.random.uniform(-0.5, 0.5)
        #y = np.random.uniform(-0.5, 0.5)
        #z = 0.0
        #move_object_to_position(name1,x,y,z)
        #x = np.random.uniform(-0.5, 0.5)
        #y = np.random.uniform(-0.5, 0.5)
        #z = 0.0
        #move_object_to_position(name2,x,y,z)
        #x = np.random.uniform(-0.5, 0.5)
        #y = np.random.uniform(-0.5, 0.5)
        #z = 0.0
        #move_object_to_position(name3,x,y,z)
        #x = np.random.uniform(-0.5, 0.5)
        #y = np.random.uniform(-0.5, 0.5)
        #z = 0.0
        #move_object_to_position(name4,x,y,z)
        
        bpy.ops.render.render()
        bpy.data.images['Render Result'].save_render(filepath = 'image'+str(i)+'.png')

