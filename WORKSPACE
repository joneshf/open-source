workspace(name = "open_source")

# Replace deprecated workspace rules
load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive",
)

cabal2bazel_commit = "771c522e0aee170b0ed488ad1bf198a5fc667ac5"
cabal2bazel_sha = "801f55f61349b95cc72bb37b9a4f00ec569a0b8d707cfa6b3be1d648c2793574"
hazel_commit = "925293994f88799ba550fd5cf3995104d1f2972c"
hazel_sha = "3a67000d037fe6e4ba276683a329b7e089fb6d7840831ade5e2c3aa54591074d"
rules_haskell_commit = "254a99edd646fa13601a7ab6a6d2c04a205260f8"
rules_haskell_sha = "02e264f6697473032388577b60da99daca5fe280bd5672e7a8230ce25e0c02db"
rules_nixpkgs_sha = "daa966ed99e906cb74d69e85b30a1e1b8ab1ef5870c2e6b05d796b4d916ed8a0"
rules_nixpkgs_version = "0.3"

http_archive(
    name = "ai_formation_hazel",
    sha256 = hazel_sha,
    strip_prefix = "hazel-{commit}".format(
        commit = hazel_commit,
    ),
    urls = [
        "https://github.com/FormationAI/hazel/archive/{commit}.tar.gz".format(
            commit = hazel_commit,
        ),
    ],
)

http_archive(
    name = "cabal2bazel",
    sha256 = cabal2bazel_sha,
    strip_prefix = "cabal2bazel-{commit}".format(
        commit = cabal2bazel_commit,
    ),
    urls = [
        "https://github.com/google/cabal2bazel/archive/{commit}.tar.gz".format(
            commit = cabal2bazel_commit,
        ),
    ],
)

http_archive(
    name = "io_tweag_rules_haskell",
    sha256 = rules_haskell_sha,
    strip_prefix = "rules_haskell-{commit}".format(
        commit = rules_haskell_commit,
    ),
    urls = [
        "https://github.com/tweag/rules_haskell/archive/{commit}.tar.gz".format(
            commit = rules_haskell_commit,
        ),
    ],
)

http_archive(
    name = "io_tweag_rules_nixpkgs",
    sha256 = rules_nixpkgs_sha,
    strip_prefix = "rules_nixpkgs-{version}".format(
        version = rules_nixpkgs_version,
    ),
    urls = [
        "https://github.com/tweag/rules_nixpkgs/archive/v{version}.tar.gz".format(
            version = rules_nixpkgs_version,
        ),
    ],
)

load(
    "@ai_formation_hazel//:hazel.bzl",
    "hazel_custom_package_hackage",
    "hazel_repositories",
)

load(
    "@io_tweag_rules_haskell//haskell:repositories.bzl",
    "haskell_repositories",
)

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_package",
)

load(
    "//:lts-12.14.bzl",
    "core_packages",
    "packages",
)

haskell_repositories()

# hazel_custom_package_hackage(
#     package_name = "cryptonite",
#     sha256 = "89be1a18af8730a7bfe4d718d7d5f6ce858e9df93a411566d15bf992db5a3c8c",
#     version = "0.25",
# )

# hazel_custom_package_hackage(
#     package_name = "zlib",
#     sha256 = "0dcc7d925769bdbeb323f83b66884101084167501f11d74d21eb9bc515707fed",
#     version = "0.6.2",
# )

hazel_repositories(
    core_packages = core_packages,
    extra_libs = {
        "pthread": "@pthread//:lib",
        "z": "@zlib//:lib",
    },
    exclude_packages = [
        # "cryptonite",
        # "zlib",
    ],
    packages = packages,
)

nixpkgs_package(
    attribute_path = "haskell.compiler.ghc843",
    build_file = "//:BUILD.ghc",
    name = "ghc",
    repositories = {
        "nixpkgs": "//:nixpkgs.nix",
    },
)


nixpkgs_package(
    attribute_path = "libpthread",
    build_file = "//:BUILD.pthread",
    name = "pthread",
    repositories = {
        "nixpkgs": "//:nixpkgs.nix",
    },
)


nixpkgs_package(
    attribute_path = "zlib",
    build_file = "//:BUILD.zlib",
    name = "zlib",
    repositories = {
        "nixpkgs": "//:nixpkgs.nix",
    },
)

register_toolchains("//:ghc")
