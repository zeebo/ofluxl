{ pkgs ? import <nixpkgs-unstable> {} }:

with pkgs; mkShell {
	buildInputs = [
		opam
		ocaml
		dune

		ocamlPackages.findlib
		ocamlPackages.merlin

		ocamlPackages.ppx_jane
		ocamlPackages.base
		ocamlPackages.menhir
	];
}