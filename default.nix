with (import ./nix {});

{
  isl-bindings = haskellPackages.isl-bindings;
  isl-test = haskellPackages.isl-test;
}
