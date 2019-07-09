
all:
	ocamlbuild -use-ocamlfind -use-menhir main.native

js:
	ocamlbuild -use-ocamlfind -use-menhir main_js.byte
	js_of_ocaml +nat.js main_js.byte

test:
	./main.native

clean:
	ocamlbuild -clean
	rm -f main_js.js
