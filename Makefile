PKGS = -pkgs ANSITerminal,integers
BUILD = ocamlbuild -use-ocamlfind -use-menhir -r $(PKGS)
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
