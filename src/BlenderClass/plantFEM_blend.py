# Blender内部のデータ構造にアクセスするために必要
import os
import sys
import bpy
sys.path.append("/home/haruka/plantFEM")
import src.PyplantFEMClass.plantFEM

# プラグインに関する情報
bl_info = {
    "name" : "plantFEM",             # プラグイン名
    "author" : "Haruka Tomobe",                  # 作者
    "version" : (0,1),                  # プラグインのバージョン
    "blender" : (2, 7, 9),              # プラグインが動作するBlenderのバージョン
    "location" : "UV Mapping > Hoge",   # Blender内部でのプラグインの位置づけ
    "description" : "plantFEM interface",   # プラグインの説明
    "warning" : "",
    "wiki_url" : "",                    # プラグインの説明が存在するWikiページのURL
    "tracker_url" : "",                 # Blender Developer OrgのスレッドURL
    "category" : "Primitive"                   # プラグインのカテゴリ名
}

# メニュー
class CHoge(bpy.types.Operator):

    bl_idname = "uv.hoge"               # ID名
    bl_label = "Hoge Menu"              # メニューに表示される文字列
    bl_description = "Hoge Piyo"        # メニューに表示される説明文
    bl_options = {'REGISTER', 'UNDO'}

    # 実際にプラグインが処理を実施する処理
    def execute(self, context):
        return {'FINISHED'}             # 成功した場合はFINISHEDを返す

# メニューを登録する関数
def menu_func(self, context):
    self.layout.operator("uv.hoge")     # 登録したいクラスの「bl_idname」を指定

# プラグインをインストールしたときの処理
def register():
    bpy.utils.register_module(__name__)
    bpy.types.VIEW3D_MT_uv_map.append(menu_func)

# プラグインをアンインストールしたときの処理
def unregister():
    bpy.utils.unregister_module(__name__)
    bpy.types.VIEW3D_MT_uv_map.remove(menu_func)

class plantFEM(bpy.ops.mesh):
    bl_idname = "plantFEM_hoge"
    bl_label  = "plantFEM_menu"
    bl_description = "plantFEM explain"
    bl_options =  {'REGISTER', 'UNDO'}

def importDomain(self):
    return {'FINISHED'}    

# メイン関数
if __name__ == "__main__":
    
    bar=plantFEM()
    bar.importDomain()