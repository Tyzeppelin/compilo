all : lexer.mll main.ml ulex.ml
	ocamlbuild -cflags -annot main.native

test : lexer.mll test.ml ulex.ml
	ocamlbuild -cflags -annot test.native

clean:
	rm -rf _build main.native
