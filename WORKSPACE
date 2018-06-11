workspace(name = "YOUR_PROJECT_NAME_HERE")

rules_haskell_rev = "02d60f544129161e63ea5b284b55f0486063f4bc"
http_archive(
    name = "io_tweag_rules_haskell",
    strip_prefix = "rules_haskell-{}".format(rules_haskell_rev),
    urls = ["https://github.com/tweag/rules_haskell/archive/{}.tar.gz".format(rules_haskell_rev)],
)

load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")
haskell_repositories()

http_archive(
  name = "io_tweag_rules_nixpkgs",
  strip_prefix = "rules_nixpkgs-0.2",
  urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.2.tar.gz"],
)

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
  "nixpkgs_git_repository",
  "nixpkgs_package"
)

nixpkgs_package(
  name = "toolchain",
  nix_file_content = """
let config = {};
    pkgs = import <nixpkgs> { config = config; }; in
{
  "islEnv" = rec {
    toolchain = pkgs.buildEnv rec {
      name = "isl-bindings-toolchain";
      extraOutputsToInstall = [ "lib" "dev" ];
      paths = with pkgs; [
        (haskell.packages.ghc841.ghcWithPackages (p: with p;
          [ base
            casing
            containers
            directory
            gcc
            language-c
            mtl
            pretty
            reflection
          ]
        ))
        isl
      ];
    };

    shell = pkgs.mkShell {
      name = "isl-bindings-shell";
      buildInputs = [toolchain];
    };
  };
}
""",
  attribute_path = "islEnv.toolchain",
  build_file_content = """
package(default_visibility = ["//visibility:public"])
filegroup(
  name = "bin",
  srcs = glob(["bin/*"]),
)

filegroup(
  name = "isl_so",
  srcs = ["lib/libisl.so"],
)

filegroup(
  name = "isl_headers",
  srcs = glob(["include/isl/**/*.h"]),
)
"""
)

register_toolchains("//:toolchain")
