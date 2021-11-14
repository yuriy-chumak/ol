import bpy
import os
import glob

# Clear scene
context = bpy.context
scene = context.scene

# Cleanup
for c in bpy.data.objects:
	bpy.data.objects.remove(c)
for collection in bpy.data.collections:
	bpy.data.collections.remove(collection)

# Model directory/files
model_dir = 'Ultimate Modular Sci-Fi - Feb 2021/OBJ/'
model_files = glob.glob(model_dir + "*.obj")

# Import obj files into scene
for f in model_files:
	bpy.ops.import_scene.obj(filepath=f,
	axis_forward='Y', axis_up='Z')

bpy.ops.wm.save_as_mainfile(filepath="Models.blend")
