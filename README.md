Build
=====

Install [Nix](https://nixos.org/nix) package manager first.

To build the default configuration, just run the `nix-build` in the root folder.

```
$ nix-build
```

To build the package using custom `nixpkgs` collection, do the following:

```
$ nix-build --argstr nixpkgs /path/to/custom/nixpkgs
```

