{ pkgs ? import <nixpkgs-unstable> {} }:

with pkgs; mkShell {
	buildInputs = [
		opam
		ocaml
		dune

		inotify-tools

		ocamlPackages.findlib
		ocamlPackages.merlin
		ocamlPackages.ocp-indent

		ocamlPackages.ppx_jane
		ocamlPackages.base
		ocamlPackages.menhir
	];
}
