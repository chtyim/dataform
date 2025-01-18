load("//tools:ts_library.bzl", "ts_library")

ts_library(
    name = "bigquery",
    srcs = [
      "ast/parse.ts",
      "ast/tree.ts",
      "common.ts",
      "lexer.ts",
      "lexer_tables.ts",
      "listener.ts",
      "parser.ts",
      "parser_tables.ts",
      "selector.ts",
      "stream.ts",
      "token.ts",
    ],
    deps = [
        "@npm//@types/node",
    ],
)

ts_library(
    name = "example",
    srcs = [ 
        "example.ts", 
    ],
    deps = [ 
        "bigquery",
    ],
)
