with (import ./nix {});

{
  isl-bindings = haskellPackages.isl-bindings;
  isl-bindings-hl = haskellPackages.isl-bindings-hl;
  isl-test = haskellPackages.isl-test;
}
