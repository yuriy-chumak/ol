"""
Blender: 259
Group: 'Export'
Tooltip: ''
https://docs.blender.org/api/current/bpy.types.BlendData.html
"""

import bpy
import math
import mathutils
import sys

print()
print("-=( exporting model data )=-------------------------")

Collection = bpy.data.collections["Collection"]

# # out = open("scene.l", "w")
# for x in Collection.objects:
# 	name = x.name

# 	if x.instance_collection:
# 		print("%s %s " %(x.name, x.instance_collection.name))
# 		print("[ %f %f %f ] " %(x.location[0], x.location[1], x.location[2]))
# 		print("[ %f %f %f ] " %(math.degrees(x.rotation_euler[0]), \
# 									math.degrees(x.rotation_euler[1]), \
# 									math.degrees(x.rotation_euler[2])))
# 		print("\n")
# # out.close()

# print("writing JSON...")
import json

def get_collection_parent(collection):
	for parent_collection in bpy.data.collections:
		if collection.name in parent_collection.children.keys():
			return parent_collection

# def obj2json(object): {
#     "name": object.name,
#     "type": object.instance_collection.name,
#     "location": [ object.location.x, object.location.y, object.location.z ],
#     "rotation": [
#         math.degrees(object.rotation_euler[0]),
#         math.degrees(object.rotation_euler[1]),
#         math.degrees(object.rotation_euler[2]) ]}

def put(array, collection):
	print("processing collection", collection)
	for object in collection.objects:
		if object.instance_collection:
			array.append({
				"name": object.name,
				"model": object.instance_collection.name,
				"location": [ object.location.x, object.location.y, object.location.z ],
				"rotation": [
					math.degrees(object.rotation_euler[0]),
					math.degrees(object.rotation_euler[1]),
					math.degrees(object.rotation_euler[2]) ]
			})
	for sub in bpy.data.collections:
		if get_collection_parent(sub) == collection:
			print("found subcollection:", sub)
			put(array, sub)

data = {}

# geometry
data["Objects"] = []
for collection in bpy.data.collections:
	if not get_collection_parent(collection) and not collection.hide_viewport:
		print (collection.name)
		# objects[collection.name] = []
		put(data["Objects"], collection) #[collection.name]

# other objects
data["Cameras"] = [];
for camera in bpy.data.cameras:
	object = bpy.data.objects[camera.name]
	target = object.matrix_world @ mathutils.Vector((0, 0, -10))
	data["Cameras"].append({
		"name": camera.type,
		"location": [ object.location.x, object.location.y, object.location.z ],
		"target": [ target.x, target.y, target.z ],
		# "rotation": [
		# 	math.degrees(object.rotation_euler[0]),
		# 	math.degrees(object.rotation_euler[1]),
		# 	math.degrees(object.rotation_euler[2]) ],
		"angle": math.degrees(camera.angle),
		"clip_start": camera.clip_start,
		"clip_end": camera.clip_end,
	})


data["Lights"] = [];
for light in bpy.data.lights:
	object = bpy.data.objects[light.name]
	if light.type == "SUN":
		target = object.matrix_world @ mathutils.Vector((0, 0, 1))
		data["Lights"].append({
			"type": light.type,
			"color": [ light.color.r, light.color.g, light.color.b ],
			# "angle": light.angle,
			# shadow_color, Color of shadows cast by the light
			# направление на "солнце"
			"position": [
				target.x - object.location.x,
				target.y - object.location.y,
				target.z - object.location.z,
				0.0 ] # обязательный 0, так как "направление"
		})
	else:
		data["Lights"].append({
			"type": light.type,
			"color": [ light.color.r, light.color.g, light.color.b ],
			"position": [
				object.location.x,
				object.location.y,
				object.location.z,
				1.0 ] # обязательная 1, так как "позиция"
		})

# objects = []
# Collection = bpy.data.collections["Collection"]
# for object in Collection.objects:
# 	if object.instance_collection:
# 		objects.append({
# 			"name": object.name,
# 			"model": object.instance_collection.name,
# 			"location": [ object.location.x, object.location.y, object.location.z ],
# 			"rotation": [
# 				math.degrees(object.rotation_euler[0]),
# 				math.degrees(object.rotation_euler[1]),
# 				math.degrees(object.rotation_euler[2]) ]
# 		})


print("json:\n")
with open('scene.json', 'w') as out:
	json.dump(data, out)
print("\ndone")
