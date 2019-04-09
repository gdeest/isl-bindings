with {
  fetch = import ./fetch.nix;
  overlays = import ./overlay.nix;
};

{ nixpkgs ? fetch.nixpkgs }:

import nixpkgs {
  config = { };
  overlays = [
    overlays.isl-bindings-overlay
    overlays.isl-test-overlay
  ];
}


