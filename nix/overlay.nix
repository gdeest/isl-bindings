let
  isl-bindings-srcgen = {pkgs}:
    let isl-codegen-src = pkgs.lib.cleanSource ../codegen;
        isl-codegen = pkgs.haskellPackages.callCabal2nix "isl-codegen" isl-codegen-src {}; in
      pkgs.stdenv.mkDerivation {
        name = "isl-bindings-srcgen";
        buildInputs = [isl-codegen pkgs.isl];

        # I don't need no source directory.
        src = pkgs.runCommandNoCC "isl-bindings-srcgen-src" {} ''mkdir $out'';

        buildPhase = ''
          isl-codegen ${pkgs.isl} .
        '';

        installPhase = ''
          mkdir $out
          cp -r * $out/
        '';
    };

  isl-bindings-src = {pkgs}:
    pkgs.stdenv.mkDerivation {
      name = "isl-bindings-src";

      # Dummy source dir.
      src = pkgs.runCommandNoCC "isl-bindings-src-src" {} ''mkdir $out'';

      buildPhase = ''
        # XXX: Need to create ./src/Isl as stupid `cp -r` won't do it.

        mkdir -p ./src/Isl
        cp -r ${pkgs.lib.cleanSource ../bindings}/* .
        cp -r ${isl-bindings-srcgen {inherit pkgs;} }/* .
      '';

      installPhase = ''
        mkdir $out
        cp -r * $out/
      '';
  };

in

{
  isl-bindings-overlay =
    (self: super: {
      haskellPackages = super.haskellPackages.extend (
          hself: hsuper: {
            isl-bindings = hsuper.callCabal2nix "isl-bindings" (isl-bindings-src {pkgs = super;}) {};

            isl-bindings-hl = hsuper.callCabal2nix "isl-bindings-hl" (self.lib.cleanSource ../highlevel) {
              isl-bindings = hself.isl-bindings;
            };
          }
        );
      }
    );

  isl-test-overlay =
    (self: super: {
      haskellPackages = super.haskellPackages.extend (
          hself: hsuper: {
            isl-test = hsuper.callCabal2nix "isl-test" (super.lib.cleanSource ../isl-test) {
              isl-bindings = hsuper.isl-bindings;
              isl-bindings-hl = hsuper.isl-bindings-hl;
            };
          }
        );
      }
    );
}

