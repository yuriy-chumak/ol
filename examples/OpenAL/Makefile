[ -d tmp ] || mkdir -p tmp


# CC0 by https://opengameart.org/content/another-space-background-track
media/ObservingTheStar.opus: tmp/ObservingTheStar.ogg
	ffmpeg -i $^ -c:a libopus -b:a 44.1K $@

tmp/ObservingTheStar.ogg: tmp/ObservingTheStar.zip
	cd tmp; unzip -o ObservingTheStar.zip

tmp/ObservingTheStar.zip: 
	cd tmp; wget -c https://opengameart.org/sites/default/files/ObservingTheStar.zip


# CC0 by https://opengameart.org/content/background-space-track
# CC0 by https://opengameart.org/content/space-music-out-there
