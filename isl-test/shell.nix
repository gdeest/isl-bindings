let
  pkgs = import ../default.nix {};

in pkgs.haskellPackages.shellFor
  {
    packages = ps: [ ps.isl-test ];
    buildInputs = [ pkgs.cabal-install pkgs.haskellPackages.ghcid ];
    withHoogle = true;
  }
