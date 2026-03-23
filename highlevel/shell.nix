let
  pkgs = import ../default.nix {};

in pkgs.haskellPackages.shellFor
  {
    packages = ps: [ ps.isl-bindings-hl ];
    buildInputs = [ pkgs.cabal-install pkgs.haskellPackages.ghcid ];
    withHoogle = true;
  }
