import bpy   # アドオン開発者に対して用意しているAPIを利用する
import os
import sys

# アドオンに関する情報を保持する、bl_info変数
bl_info = {
    "name": "createObjectSample",
    "author": "Haruka",
    "version": (2, 0),
    "blender": (2, 78, 0),
    "location": "3Dビュー > 追加 > メッシュ",
    "description": "createObjectSample",
    "warning": "",
    "support": "TESTING",
    "wiki_url": "",
    "tracker_url": "",
    "category": "Object"
}


# オブジェクト（ICO球）を生成するオペレーション
class CreateObject(bpy.types.Operator):

    bl_idname = "object.create_object"
    bl_label = "obj"
    bl_description = "creating obj"
    bl_options = {'REGISTER', 'UNDO'}

    # メニューを実行した時に呼ばれる関数
    def execute(self, context):
        bpy.ops.mesh.primitive_ico_sphere_add()
        # Add plane
        bpy.ops.mesh.primitive_plane_add()

        # Add cube
        bpy.ops.mesh.primitive_cube_add()

        # Add circle
        bpy.ops.mesh.primitive_circle_add()

        # Add UV sphere
        bpy.ops.mesh.primitive_uv_sphere_add()

        # Add Ico sphere
        bpy.ops.mesh.primitive_ico_sphere_add()

        # Add cylinder
        bpy.ops.mesh.primitive_cylinder_add()

        # Add cone
        bpy.ops.mesh.primitive_cone_add()

        # Add torus
        bpy.ops.mesh.primitive_torus_add()

        bpy.ops.mesh.primitive_cone_add(
            vertices=6,
            radius1=6,
            radius2=3,
            location=(3, 0, 0),
            rotation=(0.5, 0, 0)
        )

        for x in range(10):
            for y in range(10):
                bpy.ops.mesh.primitive_cone_add(
                    location=(x, y, 0),
                )
        print("サンプル2-1: 3Dビューにうじゃうじゃ生成しました。")
        
        return {'FINISHED'}

# オブジェクト（ICO球）を生成するオペレーション
class installSiCroF(bpy.types.Operator):

    bl_idname = "object.create_object3"
    bl_label = "install"
    bl_description = "install_SiCroF"
    bl_options = {'REGISTER', 'UNDO'}
    #bl_region_type= "TOOLS"

    # メニューを実行した時に呼ばれる関数
    def execute(self, context):
        sys.path.append("/home/haruka/SiCroF/")
        os.chdir("/home/haruka/SiCroF/")
        exec(open("/home/haruka/SiCroF/install.py").read() )
        os.system("jupyter-notebook")
        return {'FINISHED'} 

# オブジェクト（ICO球）を生成するオペレーション
class CreateObject2(bpy.types.Operator):

    bl_idname = "object.create_object2"
    bl_label = "obj2"
    bl_description = "creating obj2"
    bl_options = {'REGISTER', 'UNDO'}

    # メニューを実行した時に呼ばれる関数
    def execute(self, context):
        bpy.ops.mesh.primitive_ico_sphere_add()
        print("サンプル2-1: 3Dビューに３つICO球を生成しました。")
        
        return {'FINISHED'}


# メニューを構築する関数
def menu_fn(self, context):
    self.layout.separator()
    self.layout.operator(CreateObject.bl_idname)
    self.layout.operator(CreateObject2.bl_idname)
    self.layout.operator(installSiCroF.bl_idname)


# アドオン有効化時の処理
def register():
    bpy.utils.register_module(__name__)
    bpy.types.INFO_MT_mesh_add.append(menu_fn)
    print("サンプル2-1: アドオン「サンプル2-1」が有効化されました。")


# アドオン無効化時の処理
def unregister():
    bpy.types.INFO_MT_mesh_add.remove(menu_fn)
    bpy.utils.unregister_module(__name__)
    print("サンプル2-1: アドオン「サンプル2-1」が無効化されました。")


# メイン処理
if __name__ == "__main__":
    register()