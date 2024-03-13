all: $(patsubst %.png, %.xpm, $(wildcard *.png))

%.xpm:%.png
	convert $< $@
