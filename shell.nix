let
  pkgs = import ./default.nix {};

in pkgs.haskellPackages.shellFor
  {
    packages = ps: [
      ps.isl-bindings
      ps.isl-bindings-hl
      ps.isl-test
    ];
    buildInputs = [
      pkgs.cabal-install
      pkgs.haskellPackages.ghcid
      pkgs.isl
    ];
    withHoogle = true;
  }
