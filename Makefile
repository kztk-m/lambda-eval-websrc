ELM_FLAGS := --debug
release: ELM_FLAGS := --optimize

STATIC  := $(wildcard static/*)

debug release: all

all: dist/main.js $(STATIC:static/%=dist/%)

dist/main.js: FORCE | dist
	elm make src/Main.elm $(ELM_FLAGS) --output=$@

dist/%: static/% | dist
	cp $< $@

dist:
	mkdir -p $@

clean:
	rm -rf dist elm-stuff

FORCE:
.PHONY: all debug release clean FORCE