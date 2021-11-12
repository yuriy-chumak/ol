all: scene_.blend

resources/medieval_village_pack_-_dec_2020.zip:
	wget -c "https://opengameart.org/sites/default/files/medieval_village_pack_-_dec_2020.zip" -O "$@"
resources/Medieval\ Village\ Pack\ -\ Dec\ 2020: resources/medieval_village_pack_-_dec_2020.zip
	unzip -d resources $^ "Medieval Village Pack - Dec 2020/Buildings/OBJ/*" "Medieval Village Pack - Dec 2020/License.txt"
	cd resources; blender -b -P re-export.py; cd ..

resources/Models.blend: resources/Medieval\ Village\ Pack\ -\ Dec\ 2020
	cd resources; blender -b -P build-all.py; cd ..

scene_.blend: resources/Models.blend
	blender -b -P scene_.py
	@echo "Done."
	@echo "Now you can copy scene_blend as any filename [I used scene1.blend]"
	@echo "And start modelling via 'Add/Collection Instance...'"
	@echo "After modelling call 'make scene1.json'"

scene1.json: scene1.blend export.py
	blender -b scene1.blend -P export.py
	cat scene1.json |json_pp |tee scene1.json
