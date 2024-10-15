#include "tree_sitter/parser.h"
// NOTE: for ts_malloc, ts_calloc, ts_realloc, ts_free
// #include "tree_sitter/alloc.h"
// NOTE: for Array
// #include "tree_sitter/array.h"

enum TokenType {
    END_OF_DECLARATION,
}

struct TreeSitterGilearScanner {
    unsigned indent;
};

void *tree_sitter_gilear_external_scanner_create(void) {
  // ...
}

void tree_sitter_gilear_external_scanner_destroy(void *payload) {
  // ...
}

unsigned tree_sitter_gilear_external_scanner_serialize(
  void *payload,
  char *buffer
) {
  // ...
}

void tree_sitter_gilear_external_scanner_deserialize(
  void *payload,
  const char *buffer,
  unsigned length
) {
  // ...
}

bool tree_sitter_gilear_external_scanner_scan(
  void *payload,
  TSLexer *lexer,
  const bool *valid_symbols
) {
  // ...
}
