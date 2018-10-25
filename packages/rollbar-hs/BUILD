load(
    "@ai_formation_hazel//:hazel.bzl",
    "hazel_library",
)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_import",
    "haskell_library",
)

haskell_library(
    deps = [
        hazel_library("aeson"),
        hazel_library("base"),
        hazel_library("bytestring"),
        hazel_library("case-insensitive"),
        hazel_library("hostname"),
        hazel_library("http-client"),
        hazel_library("http-conduit"),
        hazel_library("http-types"),
        hazel_library("network"),
        hazel_library("text"),
        hazel_library("time"),
        hazel_library("unordered-containers"),
        hazel_library("uuid"),
    ],
    name = "rollbar-hs",
    srcs = glob([
        "src/**/*.hs",
    ]),
)
