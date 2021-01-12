import bpy

class TUTORIAL_OT_HELLOADDON(bpy.types.Operator):
  bl_idname = "tutorial.helloaddon"
  bl_label = "HelloAddOn"

  def execute(self, context):
    print("Hello AddOn")

    return {'FINISHED'}

bpy.utils.register_class(TUTORIAL_OT_HELLOADDON)