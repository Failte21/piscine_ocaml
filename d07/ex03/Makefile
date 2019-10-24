NAME = main

all: build_byte build_native

build_native:
	ocamlbuild ${NAME}.native

build_byte:
	ocamlbuild ${NAME}.byte

clean:
	ocamlbuild -clean

.PHONY: build_native build_byte clean
