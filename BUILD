package(default_visibility = ["//visibility:private"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_binary",
  "haskell_cc_import",
  "haskell_doc",
  "haskell_library",
  "haskell_toolchain",
)

haskell_toolchain(
  name = "toolchain",
  version = "8.4.3",
  tools = "@toolchain//:bin",
)

haskell_binary(
  name = "generate-bindings",
  srcs = ["codegen/GenerateBindings.hs"],
  main_file = "codegen/GenerateBindings.hs",
  compiler_flags = ["-threaded", "-with-rtsopts=-N", "-O1"],
  prebuilt_dependencies = [
    "base",
    "casing",
    "containers",
    "directory",
    "language-c",
    "mtl",
    "pretty",
  ],
)

genrule(
  name = "codegen",
  tools = [":generate-bindings"],
  srcs = ["@toolchain//:isl_headers"],
  outs = [
    "src/Isl/Aff/AutoGen.hs",
    "src/Isl/BasicMap/AutoGen.hs",
    "src/Isl/BasicSet/AutoGen.hs",
    "src/Isl/Constraint/AutoGen.hs",
    "src/Isl/Id/AutoGen.hs",
    "src/Isl/LocalSpace/AutoGen.hs",
    "src/Isl/Map/AutoGen.hs",
    "src/Isl/Set/AutoGen.hs",
    "src/Isl/Space/AutoGen.hs",
    "src/Isl/UnionMap/AutoGen.hs",
    "src/Isl/UnionSet/AutoGen.hs",
    "src/Isl/Val/AutoGen.hs",
  ],
  cmd = """
  $(location :generate-bindings) &&
  cp -r ./src $(@D)
""",
)

haskell_cc_import(
  name = "isl",
  hdrs = ["@toolchain//:isl_headers"],
  shared_library = "@toolchain//:isl_so",
)

haskell_library(
  name = "isl-bindings",
  visibility = ["//visibility:public"],
  deps = [":isl"],
  srcs = [
    ":codegen",
  ] + glob(["src/**/*.hs"]),
  src_strip_prefix = "src",
  prebuilt_dependencies = [
    "base",
    "reflection",
  ]
)

haskell_doc(
  name = "isl-bindings-doc",
  deps = [":isl-bindings"],
  index_transitive_deps = True,
)

haskell_binary(
  name = "simple-test",
  visibility = ["//visibility:public"],
  deps = [":isl-bindings"],
  main_file = "test/Simple.hs",
  srcs = [ "test/Simple.hs" ],
  src_strip_prefix = "test",
  compiler_flags = ["-threaded", "-with-rtsopts=-N", "-O1"],
  prebuilt_dependencies = [
    "base",
    "reflection",
  ]
)
