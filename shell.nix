{ pkgs ? import <nixpkgs-unstable> {} }:

with pkgs; mkShell {
	buildInputs = [
		opam
		ocaml
		dune

		ncurses
		inotify-tools

		ocamlPackages.utop
		ocamlPackages.findlib
		ocamlPackages.merlin
		ocamlPackages.ocp-indent

		ocamlPackages.ppx_jane
		ocamlPackages.base
		ocamlPackages.menhir
		ocamlPackages.js_of_ocaml
		ocamlPackages.js_of_ocaml-ppx
		ocamlPackages.js_of_ocaml-compiler
		ocamlPackages.ppx_tools_versioned
	];
}
