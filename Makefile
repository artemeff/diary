all:
	ocamlbuild -pkgs unix diary.native

clean:
	rm -rf ./_build
