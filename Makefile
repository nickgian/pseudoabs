PKGS = -pkgs ANSITerminal,integers,batteries
BUILD = ocamlbuild -tag 'debug'  -use-ocamlfind -use-menhir -r $(PKGS)
MLFILES= *.ml *.mli \

default:  Main

all:
	ocamlformat -i $(MLFILES)
	$(BUILD) Main.native

%: %.ml
	$(BUILD) $@.native

clean:
	ocamlbuild -clean

format:
	ocamlformat -i $(MLFILES)
