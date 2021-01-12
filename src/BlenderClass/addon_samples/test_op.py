import bpy

class  Test_OT_Operator(oby.types.Operator):
    bl_idname   = "view3d.cursor_center"
    bl_label    = "Simple Operator"
    bl_description = "center 3d cursor"

    def execute(self, context):
        bpy.ops.view3d.snap_cursor_to_center()
        return{'FINISHED'}