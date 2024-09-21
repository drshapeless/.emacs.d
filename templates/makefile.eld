makefile-mode

(clean ".PHONY: clean" n "clean:")

(sdl3-incs "$(shell pkg-config --cflags sdl3)")
(sdl3-libs "$(shell pkg-config --libs sdl3)")
