with { fetch = import ./fetch.nix; };
{ nixpkgs ? fetch.nixpkgs }:
let
  pkgs = import nixpkgs {};

  cabal-project-skeleton = pkgs.lib.cleanSource ../bindings;

  isl-codegen-src = pkgs.lib.cleanSource ../codegen;

  isl-codegen = pkgs.haskellPackages.callCabal2nix "isl-codegen" isl-codegen-src {};

  isl-bindings-srcgen = pkgs.stdenv.mkDerivation {
      name = "isl-bindings-srcgen";
      buildInputs = [isl-codegen pkgs.isl];

      # I don't need no source directory.
      src = pkgs.runCommandNoCC "isl-bindings-srcgen-src" {} ''mkdir $out'';

      buildPhase = ''
          isl-codegen ${pkgs.isl} . 
      '';

      installPhase = ''
	mkdir $out
        cp -r * $out/
      '';
  };

  isl-bindings-src = pkgs.stdenv.mkDerivation {
      name = "isl-bindings-src";

      # Dummy source dir.
      src = pkgs.runCommandNoCC "isl-bindings-src-src" {} ''mkdir $out'';

      buildPhase = ''
	# XXX: Need to create ./src/Isl as stupid `cp -r` won't do it.

	mkdir -p ./src/Isl
        cp -r ${cabal-project-skeleton}/* .
        cp -r ${isl-bindings-srcgen}/* .
      '';

      installPhase = ''
	mkdir $out
        cp -r * $out/
      '';
  };

in

import nixpkgs {
  config = { };
  overlays = [
    (self: super:
      { haskellPackages = super.haskellPackages.override {
          overrides = hself: hsuper: {
            isl-bindings = hsuper.callCabal2nix "isl-bindings" isl-bindings-src {};
          };
        };
      }
    )
  ];
}
