with {
  fetch = import ./fetch.nix;
  isl-overlays = import ./overlays.nix;
};

{ nixpkgs ? fetch.nixpkgs }:

import nixpkgs {
  config = { };
  overlays = with isl-overlays; [
    isl-bindings-overlay
    isl-test-overlay
  ];
}


