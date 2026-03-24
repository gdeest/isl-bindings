{
  description = "Haskell bindings to the Integer Set Library (ISL)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # ISL 0.27 built separately (nixpkgs ships 0.20 for GCC)
        isl_0_27 = pkgs.isl.overrideAttrs (old: rec {
          version = "0.27";
          src = pkgs.fetchurl {
            urls = [
              "mirror://sourceforge/libisl/isl-${version}.tar.xz"
              "https://libisl.sourceforge.io/isl-${version}.tar.xz"
            ];
            hash = "sha256-bYurtZ57Zy6Mt4cOh08/e4E7bgDmrz+LBPdXmWVkPVw=";
          };
        });

        # Build the code generator
        isl-codegen = pkgs.haskellPackages.callCabal2nix "isl-codegen"
          (pkgs.lib.cleanSource ./codegen) {};

        # Run the code generator against ISL 0.27 headers
        isl-bindings-srcgen = pkgs.stdenv.mkDerivation {
          name = "isl-bindings-srcgen";
          buildInputs = [ isl-codegen isl_0_27 ];
          src = pkgs.runCommand "isl-bindings-srcgen-src" {} "mkdir $out";
          buildPhase = ''
            isl-codegen ${isl_0_27} .
          '';
          installPhase = ''
            mkdir $out
            cp -r * $out/
          '';
        };

        # Combine manual sources with generated code
        isl-bindings-src = pkgs.stdenv.mkDerivation {
          name = "isl-bindings-src";
          src = pkgs.runCommand "isl-bindings-src-src" {} "mkdir $out";
          buildPhase = ''
            mkdir -p ./src/Isl
            cp -r ${pkgs.lib.cleanSource ./bindings}/* .
            cp -r ${isl-bindings-srcgen}/* .
          '';
          installPhase = ''
            mkdir $out
            cp -r * $out/
          '';
        };

        haskellPackages = pkgs.haskellPackages.extend (hself: hsuper: {
          isl-bindings = pkgs.haskell.lib.compose.addExtraLibrary isl_0_27
            (hsuper.callCabal2nix "isl-bindings" isl-bindings-src {});
          isl-bindings-hl = hsuper.callCabal2nix "isl-bindings-hl"
            (pkgs.lib.cleanSource ./highlevel) {
              isl-bindings = hself.isl-bindings;
            };
          isl-scan = hsuper.callCabal2nix "isl-scan"
            (pkgs.lib.cleanSource ./scan) {
              isl-bindings-hl = hself.isl-bindings-hl;
            };
          isl-test = hsuper.callCabal2nix "isl-test"
            (pkgs.lib.cleanSource ./isl-test) {
              isl-bindings = hself.isl-bindings;
              isl-bindings-hl = hself.isl-bindings-hl;
              isl-scan = hself.isl-scan;
            };
        });
      in
      {
        packages = {
          inherit isl-codegen;
          isl-bindings = haskellPackages.isl-bindings;
          isl-bindings-hl = haskellPackages.isl-bindings-hl;
          isl-scan = haskellPackages.isl-scan;
          isl-test = haskellPackages.isl-test;
          default = haskellPackages.isl-test;
        };

        devShells.default = haskellPackages.shellFor {
          packages = ps: [
            ps.isl-bindings
            ps.isl-bindings-hl
            ps.isl-scan
            ps.isl-test
          ];
          buildInputs = [
            pkgs.cabal-install
            pkgs.haskellPackages.ghcid
            isl_0_27
          ];
          withHoogle = true;
        };

        checks = {
          isl-test = haskellPackages.isl-test;
        };
      }
    );
}
