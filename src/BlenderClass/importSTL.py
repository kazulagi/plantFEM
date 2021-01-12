import bpy

bl_info = {
    "name" : "import_STL_object",             # プラグイン名
    "author" : "Haruka Tomobe",                  # 作者
    "version" : (0,1),                  # プラグインのバージョン
    "blender" : (2, 7, 9),              # プラグインが動作するBlenderのバージョン
    "location" : "3DVIEW > ADD > MESH ",   # Blender内部でのプラグインの位置づけ
    "description" : "importing STL object",   # プラグインの説明
    "warning" : "",
    "wiki_url" : "https://github.com/kazulagi/SiCroF",                    # プラグインの説明が存在するWikiページのURL
    "tracker_url" : "",                 # Blender Developer OrgのスレッドURL
    "support": "TESTING",
    "category" : "Object"                   # プラグインのカテゴリ名
}


## オブジェクト（ICO球）を生成するオペレーション
#class importSTL(bpy.types.Operator):
#
#    bl_idname = "object.create_object_STL"
#    bl_label = "import_STL_obj"
#    bl_description = "import STL"
#    bl_options = {'REGISTER', 'UNDO'}
#
#    # メニューを実行した時に呼ばれる関数
#    def execute(self, context):
#        #bpy.ops.import_mesh.stl()
#        bpy.ops.mesh.primitive_ico_sphere_add()
#        print("Imported STLs")
#        return{'FINISHED'}
        

# オブジェクト（ICO球）を生成するオペレーション
class CreateObjectSTL(bpy.types.Operator):

    bl_idname = "object.create_object5"
    bl_label = "objSTL"
    bl_description = "creating objSTL"
    bl_options = {'REGISTER', 'UNDO'}

    # メニューを実行した時に呼ばれる関数
    def execute(self, context):
        bpy.ops.mesh.primitive_ico_sphere_add()
        bpy.ops.export_mesh.stl(filepath="/home/haruka/test/sphere.stl")
        print("created STLs")
        
        return {'FINISHED'}

# メニューを構築する関数
def menu_fn(self, context):
    self.layout.separator()
    self.layout.operator(CreateObjectSTL.bl_idname)


# アドオン有効化時の処理
def register():
    bpy.utils.register_module(__name__)
    bpy.types.INFO_MT_mesh_add.append(menu_fn)
    print("Addon importSTL is activated [ok]")


# アドオン無効化時の処理
def unregister():
    bpy.types.INFO_MT_mesh_add.remove(menu_fn)
    bpy.utils.unregister_module(__name__)
    print("Addon importSTL is inactivated [ok]")


# メイン処理
if __name__ == "__main__":
    register()