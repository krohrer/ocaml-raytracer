run:
	ocamlbuild -use-ocamlfind -pkg graphics raytracer.native && ./raytracer.native

top:
	ocamlbuild -use-ocamlfind -pkg graphics raytracer.top
