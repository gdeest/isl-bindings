# What is it ?

This project contains low-level (mostly complete, auto-generated) and high-level
(incomplete, manually written) bindings to the
[isl](http://isl.gforge.inria.fr/user.html) library.

The end goal is to make it easy to manipulate polyhedral sets and relations in
Haskell. Envisioned applications include static analysis, automatic
parallelization, cache optimization, tuning of implementations, hardware
generation and the like. The high-level interface currently allows defining
basic sets like this:

```haskell
someSet :: forall s. HasCtx s => BasicSet s 2
someSet = BS.mkBasicSet $
  \(x :- y :- Nil) ->
    idx x >=: cst 0 &&: idx x <=: cst 100 &&:
    idx y >=: idx x &&: idx y <=: cst 100
```

which is an order of magnitude shorter than the corresponding C code! Also, we
try very hard to make the API as safe as possible. For example:

- The `BasicSet` type is indexed by a type variable `s` corresponding to the
  `isl` context, as well as by its number of dimensions.
- `BS.mkBasicSet` uses the same rank-2 type trick as the ST monad to guarantee
  that indices cannot exit their scope.

*DISCLAIMER:* This software is unsupported and of experimental quality only. It
may or may not work for your use-case.

That being said, please try it out, break it and file bugs !

# How to use it ?

This software is built and packaged with the [Nix](https://nixos.org) package
manager. The recommended way to use this library is to consume it as a Nix
overlay. For example, put this in `<some-directory>/default.nix`:

```nix
with rec {
  nixpkgs-rev = "67bc63f9a7ac1b4d1a7114c88ca1a4df03bfdb0e";
  pkgs = builtins.fetchTarball {
    name = "nixos-19.03";
    url = "https://github.com/NixOS/nixpkgs/archive/${nixpkgs-rev}.tar.gz";
    sha256 = "0jzy9kd81dz1v0by3h0znz3z6bmpll3ssza5i5f14j2q54ib145g";
  };
 
  isl-bindings-rev = "f462de33568a6fe0edc1b6d08da453a4c23cda2b";
  isl-bindings = builtins.fetchGit {
    name = "isl-bindings";
    url = "https://github.com/gdeest/isl-bindings";
    rev = isl-bindings-rev;
  };
};
 
import pkgs {
  overlays = [ (import isl-bindings) ];
}
```

and this in `<some-directory/shell.nix>`:

```nix
with import ./.;

mkShell {
  name = "isl-dev-env";
  buildInputs = [
    (haskellPackages.ghcWithPackages(ps: with ps; [isl-bindings isl-bindings-hl]))
    cabal-install
  ];
}
```

Typing `nix-shell` within that directory will drop you in a shell environment
with Cabal, ghc and isl-bindings available ! We can check that everything is fine with:

```
$ ghci -package isl-bindings-hl
```

If `ghci` loads successfully, you are good to go !
