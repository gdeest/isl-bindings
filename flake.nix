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
            chmod -R u+w .
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
          isl-typelevel = hsuper.callCabal2nix "isl-typelevel"
            (pkgs.lib.cleanSource ./typelevel) {
              isl-bindings = hself.isl-bindings;
            };
          isl-plugin = hsuper.callCabal2nix "isl-plugin"
            (pkgs.lib.cleanSource ./plugin) {
              isl-bindings = hself.isl-bindings;
              isl-typelevel = hself.isl-typelevel;
            };
          isl-alpha = hsuper.callCabal2nix "isl-alpha"
            (pkgs.lib.cleanSource ./alpha) {
              isl-bindings = hself.isl-bindings;
              isl-typelevel = hself.isl-typelevel;
              isl-plugin = hself.isl-plugin;
            };
        });
      in
      {
        packages = {
          inherit isl-codegen;
          isl-bindings = haskellPackages.isl-bindings;
          isl-typelevel = haskellPackages.isl-typelevel;
          isl-plugin = haskellPackages.isl-plugin;
          isl-alpha = haskellPackages.isl-alpha;
          default = haskellPackages.isl-alpha;
        };

        devShells.default = haskellPackages.shellFor {
          packages = ps: [
            ps.isl-bindings
            ps.isl-typelevel
            ps.isl-plugin
            ps.isl-alpha
          ];
          buildInputs = [
            pkgs.cabal-install
            pkgs.haskellPackages.ghcid
            pkgs.haskellPackages.haskell-language-server
            pkgs.haskellPackages.weeder
            isl_0_27
            pkgs.z3
          ];
          withHoogle = true;
        };

        checks = {
          isl-alpha = haskellPackages.isl-alpha;
        };
      }
    );
}
