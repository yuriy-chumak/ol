import bpy
import os
import glob

# Clear scene
context = bpy.context
scene = context.scene

bpy.data.objects.remove(bpy.data.objects["Cube"]) # Remove startup Cube

# Model directory/files
blend_file = "resources/Models.blend"
section = "\\Action\\"

Gamedata = bpy.data.collections.new("Gamedata")
Gamedata.hide_viewport = True
bpy.context.scene.collection.children.link(Gamedata)

# prepare files list for bpy.ops.wm.append(..)
files = []
with bpy.data.libraries.load(blend_file) as (data_from, data_to):
	for name in data_from.objects:
		files.append({'name': name})

bpy.ops.wm.append(directory=blend_file + "/Object/"
	, filename = name
	, link = True
	, files = files)

Collection = bpy.data.collections['Collection']

for x in bpy.data.objects[2:]: # no 'Camera' and 'Light'
	name = x.name
	collection = bpy.data.collections.new(name)
	Gamedata.children.link(collection)
	collection.objects.link(x)
	Collection.objects.unlink(x)

bpy.ops.wm.save_as_mainfile(filepath="scene_.blend")
print("Ok.")
