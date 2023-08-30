.PNOHY: build-all
build-all:
	dune build @all

.PHONY: swith-setup
switch-setup:
	opam switch create ./ --deps-only 5.0.0 --with-test -y

.PHONY: dev-setup
dev-setup:
	opam install ocaml-lsp-server ocamlformat -y 

.PHONY: main-watch
main-watch:
	dune exec --watch bin/main.exe

.PHONY: test-watch
test-watch:
	dune runtest --watch --auto-promote

.PNOHY: fmt
fmt:
	dune build @fmt
