#include "tree_sitter/parser.h"

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 14
#define STATE_COUNT 40
#define LARGE_STATE_COUNT 10
#define SYMBOL_COUNT 29
#define ALIAS_COUNT 0
#define TOKEN_COUNT 10
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 5
#define MAX_ALIAS_SEQUENCE_LENGTH 5
#define PRODUCTION_ID_COUNT 6

enum ts_symbol_identifiers {
  anon_sym_COLON = 1,
  anon_sym_SEMI = 2,
  anon_sym_EQ = 3,
  anon_sym_LPAREN = 4,
  anon_sym_RPAREN = 5,
  anon_sym_DASH_GT = 6,
  anon_sym_BSLASH = 7,
  sym_variable_name = 8,
  sym_constructor_name = 9,
  sym_source_file = 10,
  sym_declaration = 11,
  sym_declaration_type_signature = 12,
  sym_declaration_function = 13,
  sym_pattern_list = 14,
  sym_pattern = 15,
  sym_pattern_variable = 16,
  sym_pattern_constructor = 17,
  sym_pattern_parentheses = 18,
  sym_expression = 19,
  sym_expression_variable = 20,
  sym_expression_constructor = 21,
  sym_expression_function_type = 22,
  sym_expression_function_application = 23,
  sym_expression_function_abstraction = 24,
  sym_expression_annotation = 25,
  sym_expression_parentheses = 26,
  aux_sym_source_file_repeat1 = 27,
  aux_sym_pattern_list_repeat1 = 28,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [anon_sym_COLON] = ":",
  [anon_sym_SEMI] = ";",
  [anon_sym_EQ] = "=",
  [anon_sym_LPAREN] = "(",
  [anon_sym_RPAREN] = ")",
  [anon_sym_DASH_GT] = "->",
  [anon_sym_BSLASH] = "\\",
  [sym_variable_name] = "variable_name",
  [sym_constructor_name] = "constructor_name",
  [sym_source_file] = "source_file",
  [sym_declaration] = "declaration",
  [sym_declaration_type_signature] = "declaration_type_signature",
  [sym_declaration_function] = "declaration_function",
  [sym_pattern_list] = "pattern_list",
  [sym_pattern] = "pattern",
  [sym_pattern_variable] = "pattern_variable",
  [sym_pattern_constructor] = "pattern_constructor",
  [sym_pattern_parentheses] = "pattern_parentheses",
  [sym_expression] = "expression",
  [sym_expression_variable] = "expression_variable",
  [sym_expression_constructor] = "expression_constructor",
  [sym_expression_function_type] = "expression_function_type",
  [sym_expression_function_application] = "expression_function_application",
  [sym_expression_function_abstraction] = "expression_function_abstraction",
  [sym_expression_annotation] = "expression_annotation",
  [sym_expression_parentheses] = "expression_parentheses",
  [aux_sym_source_file_repeat1] = "source_file_repeat1",
  [aux_sym_pattern_list_repeat1] = "pattern_list_repeat1",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [anon_sym_COLON] = anon_sym_COLON,
  [anon_sym_SEMI] = anon_sym_SEMI,
  [anon_sym_EQ] = anon_sym_EQ,
  [anon_sym_LPAREN] = anon_sym_LPAREN,
  [anon_sym_RPAREN] = anon_sym_RPAREN,
  [anon_sym_DASH_GT] = anon_sym_DASH_GT,
  [anon_sym_BSLASH] = anon_sym_BSLASH,
  [sym_variable_name] = sym_variable_name,
  [sym_constructor_name] = sym_constructor_name,
  [sym_source_file] = sym_source_file,
  [sym_declaration] = sym_declaration,
  [sym_declaration_type_signature] = sym_declaration_type_signature,
  [sym_declaration_function] = sym_declaration_function,
  [sym_pattern_list] = sym_pattern_list,
  [sym_pattern] = sym_pattern,
  [sym_pattern_variable] = sym_pattern_variable,
  [sym_pattern_constructor] = sym_pattern_constructor,
  [sym_pattern_parentheses] = sym_pattern_parentheses,
  [sym_expression] = sym_expression,
  [sym_expression_variable] = sym_expression_variable,
  [sym_expression_constructor] = sym_expression_constructor,
  [sym_expression_function_type] = sym_expression_function_type,
  [sym_expression_function_application] = sym_expression_function_application,
  [sym_expression_function_abstraction] = sym_expression_function_abstraction,
  [sym_expression_annotation] = sym_expression_annotation,
  [sym_expression_parentheses] = sym_expression_parentheses,
  [aux_sym_source_file_repeat1] = aux_sym_source_file_repeat1,
  [aux_sym_pattern_list_repeat1] = aux_sym_pattern_list_repeat1,
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
  },
  [anon_sym_COLON] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_SEMI] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DASH_GT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_BSLASH] = {
    .visible = true,
    .named = false,
  },
  [sym_variable_name] = {
    .visible = true,
    .named = true,
  },
  [sym_constructor_name] = {
    .visible = true,
    .named = true,
  },
  [sym_source_file] = {
    .visible = true,
    .named = true,
  },
  [sym_declaration] = {
    .visible = false,
    .named = true,
    .supertype = true,
  },
  [sym_declaration_type_signature] = {
    .visible = true,
    .named = true,
  },
  [sym_declaration_function] = {
    .visible = true,
    .named = true,
  },
  [sym_pattern_list] = {
    .visible = true,
    .named = true,
  },
  [sym_pattern] = {
    .visible = false,
    .named = true,
    .supertype = true,
  },
  [sym_pattern_variable] = {
    .visible = true,
    .named = true,
  },
  [sym_pattern_constructor] = {
    .visible = true,
    .named = true,
  },
  [sym_pattern_parentheses] = {
    .visible = true,
    .named = true,
  },
  [sym_expression] = {
    .visible = false,
    .named = true,
    .supertype = true,
  },
  [sym_expression_variable] = {
    .visible = true,
    .named = true,
  },
  [sym_expression_constructor] = {
    .visible = true,
    .named = true,
  },
  [sym_expression_function_type] = {
    .visible = true,
    .named = true,
  },
  [sym_expression_function_application] = {
    .visible = true,
    .named = true,
  },
  [sym_expression_function_abstraction] = {
    .visible = true,
    .named = true,
  },
  [sym_expression_annotation] = {
    .visible = true,
    .named = true,
  },
  [sym_expression_parentheses] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_source_file_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_pattern_list_repeat1] = {
    .visible = false,
    .named = false,
  },
};

enum ts_field_identifiers {
  field_arg = 1,
  field_fun = 2,
  field_lhs = 3,
  field_name = 4,
  field_rhs = 5,
};

static const char * const ts_field_names[] = {
  [0] = NULL,
  [field_arg] = "arg",
  [field_fun] = "fun",
  [field_lhs] = "lhs",
  [field_name] = "name",
  [field_rhs] = "rhs",
};

static const TSFieldMapSlice ts_field_map_slices[PRODUCTION_ID_COUNT] = {
  [1] = {.index = 0, .length = 2},
  [2] = {.index = 2, .length = 2},
  [3] = {.index = 4, .length = 2},
  [4] = {.index = 6, .length = 3},
  [5] = {.index = 9, .length = 2},
};

static const TSFieldMapEntry ts_field_map_entries[] = {
  [0] =
    {field_name, 0},
    {field_rhs, 2},
  [2] =
    {field_arg, 1},
    {field_fun, 0},
  [4] =
    {field_lhs, 0},
    {field_rhs, 2},
  [6] =
    {field_lhs, 1},
    {field_name, 0},
    {field_rhs, 3},
  [9] =
    {field_lhs, 1},
    {field_rhs, 3},
};

static const TSSymbol ts_alias_sequences[PRODUCTION_ID_COUNT][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
};

static const uint16_t ts_non_terminal_alias_map[] = {
  0,
};

static const TSStateId ts_primary_state_ids[STATE_COUNT] = {
  [0] = 0,
  [1] = 1,
  [2] = 2,
  [3] = 3,
  [4] = 4,
  [5] = 5,
  [6] = 6,
  [7] = 7,
  [8] = 8,
  [9] = 9,
  [10] = 10,
  [11] = 11,
  [12] = 12,
  [13] = 13,
  [14] = 14,
  [15] = 15,
  [16] = 16,
  [17] = 17,
  [18] = 18,
  [19] = 19,
  [20] = 20,
  [21] = 21,
  [22] = 22,
  [23] = 23,
  [24] = 24,
  [25] = 25,
  [26] = 26,
  [27] = 27,
  [28] = 28,
  [29] = 29,
  [30] = 30,
  [31] = 31,
  [32] = 32,
  [33] = 33,
  [34] = 34,
  [35] = 35,
  [36] = 36,
  [37] = 37,
  [38] = 38,
  [39] = 39,
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(2);
      if (lookahead == '(') ADVANCE(6);
      if (lookahead == ')') ADVANCE(7);
      if (lookahead == '-') ADVANCE(1);
      if (lookahead == ':') ADVANCE(3);
      if (lookahead == ';') ADVANCE(4);
      if (lookahead == '=') ADVANCE(5);
      if (lookahead == '\\') ADVANCE(9);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(0);
      if (('A' <= lookahead && lookahead <= 'Z')) ADVANCE(11);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(10);
      END_STATE();
    case 1:
      if (lookahead == '>') ADVANCE(8);
      END_STATE();
    case 2:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 3:
      ACCEPT_TOKEN(anon_sym_COLON);
      END_STATE();
    case 4:
      ACCEPT_TOKEN(anon_sym_SEMI);
      END_STATE();
    case 5:
      ACCEPT_TOKEN(anon_sym_EQ);
      END_STATE();
    case 6:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 7:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 8:
      ACCEPT_TOKEN(anon_sym_DASH_GT);
      END_STATE();
    case 9:
      ACCEPT_TOKEN(anon_sym_BSLASH);
      END_STATE();
    case 10:
      ACCEPT_TOKEN(sym_variable_name);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(10);
      END_STATE();
    case 11:
      ACCEPT_TOKEN(sym_constructor_name);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(11);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 0},
  [2] = {.lex_state = 0},
  [3] = {.lex_state = 0},
  [4] = {.lex_state = 0},
  [5] = {.lex_state = 0},
  [6] = {.lex_state = 0},
  [7] = {.lex_state = 0},
  [8] = {.lex_state = 0},
  [9] = {.lex_state = 0},
  [10] = {.lex_state = 0},
  [11] = {.lex_state = 0},
  [12] = {.lex_state = 0},
  [13] = {.lex_state = 0},
  [14] = {.lex_state = 0},
  [15] = {.lex_state = 0},
  [16] = {.lex_state = 0},
  [17] = {.lex_state = 0},
  [18] = {.lex_state = 0},
  [19] = {.lex_state = 0},
  [20] = {.lex_state = 0},
  [21] = {.lex_state = 0},
  [22] = {.lex_state = 0},
  [23] = {.lex_state = 0},
  [24] = {.lex_state = 0},
  [25] = {.lex_state = 0},
  [26] = {.lex_state = 0},
  [27] = {.lex_state = 0},
  [28] = {.lex_state = 0},
  [29] = {.lex_state = 0},
  [30] = {.lex_state = 0},
  [31] = {.lex_state = 0},
  [32] = {.lex_state = 0},
  [33] = {.lex_state = 0},
  [34] = {.lex_state = 0},
  [35] = {.lex_state = 0},
  [36] = {.lex_state = 0},
  [37] = {.lex_state = 0},
  [38] = {.lex_state = 0},
  [39] = {.lex_state = 0},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [anon_sym_COLON] = ACTIONS(1),
    [anon_sym_SEMI] = ACTIONS(1),
    [anon_sym_EQ] = ACTIONS(1),
    [anon_sym_LPAREN] = ACTIONS(1),
    [anon_sym_RPAREN] = ACTIONS(1),
    [anon_sym_DASH_GT] = ACTIONS(1),
    [anon_sym_BSLASH] = ACTIONS(1),
    [sym_variable_name] = ACTIONS(1),
    [sym_constructor_name] = ACTIONS(1),
  },
  [1] = {
    [sym_source_file] = STATE(36),
    [sym_declaration] = STATE(30),
    [sym_declaration_type_signature] = STATE(33),
    [sym_declaration_function] = STATE(33),
    [aux_sym_source_file_repeat1] = STATE(30),
    [ts_builtin_sym_end] = ACTIONS(3),
    [sym_variable_name] = ACTIONS(5),
  },
  [2] = {
    [sym_expression] = STATE(2),
    [sym_expression_variable] = STATE(23),
    [sym_expression_constructor] = STATE(23),
    [sym_expression_function_type] = STATE(23),
    [sym_expression_function_application] = STATE(23),
    [sym_expression_function_abstraction] = STATE(23),
    [sym_expression_annotation] = STATE(23),
    [sym_expression_parentheses] = STATE(23),
    [anon_sym_COLON] = ACTIONS(7),
    [anon_sym_SEMI] = ACTIONS(7),
    [anon_sym_LPAREN] = ACTIONS(7),
    [anon_sym_RPAREN] = ACTIONS(7),
    [anon_sym_DASH_GT] = ACTIONS(7),
    [anon_sym_BSLASH] = ACTIONS(7),
    [sym_variable_name] = ACTIONS(7),
    [sym_constructor_name] = ACTIONS(7),
  },
  [3] = {
    [sym_expression] = STATE(2),
    [sym_expression_variable] = STATE(23),
    [sym_expression_constructor] = STATE(23),
    [sym_expression_function_type] = STATE(23),
    [sym_expression_function_application] = STATE(23),
    [sym_expression_function_abstraction] = STATE(23),
    [sym_expression_annotation] = STATE(23),
    [sym_expression_parentheses] = STATE(23),
    [anon_sym_COLON] = ACTIONS(9),
    [anon_sym_SEMI] = ACTIONS(11),
    [anon_sym_LPAREN] = ACTIONS(13),
    [anon_sym_RPAREN] = ACTIONS(11),
    [anon_sym_DASH_GT] = ACTIONS(15),
    [anon_sym_BSLASH] = ACTIONS(17),
    [sym_variable_name] = ACTIONS(19),
    [sym_constructor_name] = ACTIONS(21),
  },
  [4] = {
    [sym_expression] = STATE(2),
    [sym_expression_variable] = STATE(23),
    [sym_expression_constructor] = STATE(23),
    [sym_expression_function_type] = STATE(23),
    [sym_expression_function_application] = STATE(23),
    [sym_expression_function_abstraction] = STATE(23),
    [sym_expression_annotation] = STATE(23),
    [sym_expression_parentheses] = STATE(23),
    [anon_sym_COLON] = ACTIONS(9),
    [anon_sym_SEMI] = ACTIONS(23),
    [anon_sym_LPAREN] = ACTIONS(13),
    [anon_sym_RPAREN] = ACTIONS(23),
    [anon_sym_DASH_GT] = ACTIONS(15),
    [anon_sym_BSLASH] = ACTIONS(17),
    [sym_variable_name] = ACTIONS(19),
    [sym_constructor_name] = ACTIONS(21),
  },
  [5] = {
    [sym_expression] = STATE(2),
    [sym_expression_variable] = STATE(23),
    [sym_expression_constructor] = STATE(23),
    [sym_expression_function_type] = STATE(23),
    [sym_expression_function_application] = STATE(23),
    [sym_expression_function_abstraction] = STATE(23),
    [sym_expression_annotation] = STATE(23),
    [sym_expression_parentheses] = STATE(23),
    [anon_sym_COLON] = ACTIONS(9),
    [anon_sym_SEMI] = ACTIONS(25),
    [anon_sym_LPAREN] = ACTIONS(13),
    [anon_sym_RPAREN] = ACTIONS(25),
    [anon_sym_DASH_GT] = ACTIONS(15),
    [anon_sym_BSLASH] = ACTIONS(17),
    [sym_variable_name] = ACTIONS(19),
    [sym_constructor_name] = ACTIONS(21),
  },
  [6] = {
    [sym_expression] = STATE(2),
    [sym_expression_variable] = STATE(23),
    [sym_expression_constructor] = STATE(23),
    [sym_expression_function_type] = STATE(23),
    [sym_expression_function_application] = STATE(23),
    [sym_expression_function_abstraction] = STATE(23),
    [sym_expression_annotation] = STATE(23),
    [sym_expression_parentheses] = STATE(23),
    [anon_sym_COLON] = ACTIONS(9),
    [anon_sym_SEMI] = ACTIONS(27),
    [anon_sym_LPAREN] = ACTIONS(13),
    [anon_sym_DASH_GT] = ACTIONS(15),
    [anon_sym_BSLASH] = ACTIONS(17),
    [sym_variable_name] = ACTIONS(19),
    [sym_constructor_name] = ACTIONS(21),
  },
  [7] = {
    [sym_expression] = STATE(2),
    [sym_expression_variable] = STATE(23),
    [sym_expression_constructor] = STATE(23),
    [sym_expression_function_type] = STATE(23),
    [sym_expression_function_application] = STATE(23),
    [sym_expression_function_abstraction] = STATE(23),
    [sym_expression_annotation] = STATE(23),
    [sym_expression_parentheses] = STATE(23),
    [anon_sym_COLON] = ACTIONS(9),
    [anon_sym_SEMI] = ACTIONS(29),
    [anon_sym_LPAREN] = ACTIONS(13),
    [anon_sym_DASH_GT] = ACTIONS(15),
    [anon_sym_BSLASH] = ACTIONS(17),
    [sym_variable_name] = ACTIONS(19),
    [sym_constructor_name] = ACTIONS(21),
  },
  [8] = {
    [sym_expression] = STATE(2),
    [sym_expression_variable] = STATE(23),
    [sym_expression_constructor] = STATE(23),
    [sym_expression_function_type] = STATE(23),
    [sym_expression_function_application] = STATE(23),
    [sym_expression_function_abstraction] = STATE(23),
    [sym_expression_annotation] = STATE(23),
    [sym_expression_parentheses] = STATE(23),
    [anon_sym_COLON] = ACTIONS(9),
    [anon_sym_SEMI] = ACTIONS(31),
    [anon_sym_LPAREN] = ACTIONS(13),
    [anon_sym_DASH_GT] = ACTIONS(15),
    [anon_sym_BSLASH] = ACTIONS(17),
    [sym_variable_name] = ACTIONS(19),
    [sym_constructor_name] = ACTIONS(21),
  },
  [9] = {
    [sym_expression] = STATE(2),
    [sym_expression_variable] = STATE(23),
    [sym_expression_constructor] = STATE(23),
    [sym_expression_function_type] = STATE(23),
    [sym_expression_function_application] = STATE(23),
    [sym_expression_function_abstraction] = STATE(23),
    [sym_expression_annotation] = STATE(23),
    [sym_expression_parentheses] = STATE(23),
    [anon_sym_COLON] = ACTIONS(9),
    [anon_sym_LPAREN] = ACTIONS(13),
    [anon_sym_RPAREN] = ACTIONS(33),
    [anon_sym_DASH_GT] = ACTIONS(15),
    [anon_sym_BSLASH] = ACTIONS(17),
    [sym_variable_name] = ACTIONS(19),
    [sym_constructor_name] = ACTIONS(21),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 6,
    ACTIONS(13), 1,
      anon_sym_LPAREN,
    ACTIONS(17), 1,
      anon_sym_BSLASH,
    ACTIONS(19), 1,
      sym_variable_name,
    ACTIONS(21), 1,
      sym_constructor_name,
    STATE(5), 1,
      sym_expression,
    STATE(23), 7,
      sym_expression_variable,
      sym_expression_constructor,
      sym_expression_function_type,
      sym_expression_function_application,
      sym_expression_function_abstraction,
      sym_expression_annotation,
      sym_expression_parentheses,
  [25] = 6,
    ACTIONS(13), 1,
      anon_sym_LPAREN,
    ACTIONS(17), 1,
      anon_sym_BSLASH,
    ACTIONS(19), 1,
      sym_variable_name,
    ACTIONS(21), 1,
      sym_constructor_name,
    STATE(7), 1,
      sym_expression,
    STATE(23), 7,
      sym_expression_variable,
      sym_expression_constructor,
      sym_expression_function_type,
      sym_expression_function_application,
      sym_expression_function_abstraction,
      sym_expression_annotation,
      sym_expression_parentheses,
  [50] = 6,
    ACTIONS(13), 1,
      anon_sym_LPAREN,
    ACTIONS(17), 1,
      anon_sym_BSLASH,
    ACTIONS(19), 1,
      sym_variable_name,
    ACTIONS(21), 1,
      sym_constructor_name,
    STATE(9), 1,
      sym_expression,
    STATE(23), 7,
      sym_expression_variable,
      sym_expression_constructor,
      sym_expression_function_type,
      sym_expression_function_application,
      sym_expression_function_abstraction,
      sym_expression_annotation,
      sym_expression_parentheses,
  [75] = 6,
    ACTIONS(13), 1,
      anon_sym_LPAREN,
    ACTIONS(17), 1,
      anon_sym_BSLASH,
    ACTIONS(19), 1,
      sym_variable_name,
    ACTIONS(21), 1,
      sym_constructor_name,
    STATE(3), 1,
      sym_expression,
    STATE(23), 7,
      sym_expression_variable,
      sym_expression_constructor,
      sym_expression_function_type,
      sym_expression_function_application,
      sym_expression_function_abstraction,
      sym_expression_annotation,
      sym_expression_parentheses,
  [100] = 6,
    ACTIONS(13), 1,
      anon_sym_LPAREN,
    ACTIONS(17), 1,
      anon_sym_BSLASH,
    ACTIONS(19), 1,
      sym_variable_name,
    ACTIONS(21), 1,
      sym_constructor_name,
    STATE(6), 1,
      sym_expression,
    STATE(23), 7,
      sym_expression_variable,
      sym_expression_constructor,
      sym_expression_function_type,
      sym_expression_function_application,
      sym_expression_function_abstraction,
      sym_expression_annotation,
      sym_expression_parentheses,
  [125] = 6,
    ACTIONS(13), 1,
      anon_sym_LPAREN,
    ACTIONS(17), 1,
      anon_sym_BSLASH,
    ACTIONS(19), 1,
      sym_variable_name,
    ACTIONS(21), 1,
      sym_constructor_name,
    STATE(4), 1,
      sym_expression,
    STATE(23), 7,
      sym_expression_variable,
      sym_expression_constructor,
      sym_expression_function_type,
      sym_expression_function_application,
      sym_expression_function_abstraction,
      sym_expression_annotation,
      sym_expression_parentheses,
  [150] = 6,
    ACTIONS(13), 1,
      anon_sym_LPAREN,
    ACTIONS(17), 1,
      anon_sym_BSLASH,
    ACTIONS(19), 1,
      sym_variable_name,
    ACTIONS(21), 1,
      sym_constructor_name,
    STATE(8), 1,
      sym_expression,
    STATE(23), 7,
      sym_expression_variable,
      sym_expression_constructor,
      sym_expression_function_type,
      sym_expression_function_application,
      sym_expression_function_abstraction,
      sym_expression_annotation,
      sym_expression_parentheses,
  [175] = 8,
    ACTIONS(35), 1,
      anon_sym_COLON,
    ACTIONS(37), 1,
      anon_sym_EQ,
    ACTIONS(39), 1,
      anon_sym_LPAREN,
    ACTIONS(41), 1,
      sym_variable_name,
    ACTIONS(43), 1,
      sym_constructor_name,
    STATE(38), 1,
      sym_pattern_list,
    STATE(18), 2,
      sym_pattern,
      aux_sym_pattern_list_repeat1,
    STATE(26), 3,
      sym_pattern_variable,
      sym_pattern_constructor,
      sym_pattern_parentheses,
  [203] = 6,
    ACTIONS(39), 1,
      anon_sym_LPAREN,
    ACTIONS(41), 1,
      sym_variable_name,
    ACTIONS(43), 1,
      sym_constructor_name,
    ACTIONS(45), 2,
      anon_sym_EQ,
      anon_sym_DASH_GT,
    STATE(19), 2,
      sym_pattern,
      aux_sym_pattern_list_repeat1,
    STATE(26), 3,
      sym_pattern_variable,
      sym_pattern_constructor,
      sym_pattern_parentheses,
  [226] = 6,
    ACTIONS(49), 1,
      anon_sym_LPAREN,
    ACTIONS(52), 1,
      sym_variable_name,
    ACTIONS(55), 1,
      sym_constructor_name,
    ACTIONS(47), 2,
      anon_sym_EQ,
      anon_sym_DASH_GT,
    STATE(19), 2,
      sym_pattern,
      aux_sym_pattern_list_repeat1,
    STATE(26), 3,
      sym_pattern_variable,
      sym_pattern_constructor,
      sym_pattern_parentheses,
  [249] = 6,
    ACTIONS(39), 1,
      anon_sym_LPAREN,
    ACTIONS(41), 1,
      sym_variable_name,
    ACTIONS(43), 1,
      sym_constructor_name,
    STATE(39), 1,
      sym_pattern_list,
    STATE(18), 2,
      sym_pattern,
      aux_sym_pattern_list_repeat1,
    STATE(26), 3,
      sym_pattern_variable,
      sym_pattern_constructor,
      sym_pattern_parentheses,
  [271] = 1,
    ACTIONS(58), 8,
      anon_sym_COLON,
      anon_sym_SEMI,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DASH_GT,
      anon_sym_BSLASH,
      sym_variable_name,
      sym_constructor_name,
  [282] = 1,
    ACTIONS(60), 8,
      anon_sym_COLON,
      anon_sym_SEMI,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DASH_GT,
      anon_sym_BSLASH,
      sym_variable_name,
      sym_constructor_name,
  [293] = 1,
    ACTIONS(62), 8,
      anon_sym_COLON,
      anon_sym_SEMI,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DASH_GT,
      anon_sym_BSLASH,
      sym_variable_name,
      sym_constructor_name,
  [304] = 1,
    ACTIONS(64), 8,
      anon_sym_COLON,
      anon_sym_SEMI,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DASH_GT,
      anon_sym_BSLASH,
      sym_variable_name,
      sym_constructor_name,
  [315] = 5,
    ACTIONS(39), 1,
      anon_sym_LPAREN,
    ACTIONS(41), 1,
      sym_variable_name,
    ACTIONS(43), 1,
      sym_constructor_name,
    STATE(37), 1,
      sym_pattern,
    STATE(26), 3,
      sym_pattern_variable,
      sym_pattern_constructor,
      sym_pattern_parentheses,
  [333] = 1,
    ACTIONS(66), 6,
      anon_sym_EQ,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DASH_GT,
      sym_variable_name,
      sym_constructor_name,
  [342] = 4,
    ACTIONS(68), 1,
      ts_builtin_sym_end,
    ACTIONS(70), 1,
      sym_variable_name,
    STATE(27), 2,
      sym_declaration,
      aux_sym_source_file_repeat1,
    STATE(33), 2,
      sym_declaration_type_signature,
      sym_declaration_function,
  [357] = 1,
    ACTIONS(73), 6,
      anon_sym_EQ,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DASH_GT,
      sym_variable_name,
      sym_constructor_name,
  [366] = 1,
    ACTIONS(75), 6,
      anon_sym_EQ,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DASH_GT,
      sym_variable_name,
      sym_constructor_name,
  [375] = 4,
    ACTIONS(5), 1,
      sym_variable_name,
    ACTIONS(77), 1,
      ts_builtin_sym_end,
    STATE(27), 2,
      sym_declaration,
      aux_sym_source_file_repeat1,
    STATE(33), 2,
      sym_declaration_type_signature,
      sym_declaration_function,
  [390] = 1,
    ACTIONS(79), 6,
      anon_sym_EQ,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DASH_GT,
      sym_variable_name,
      sym_constructor_name,
  [399] = 1,
    ACTIONS(81), 2,
      ts_builtin_sym_end,
      sym_variable_name,
  [404] = 1,
    ACTIONS(83), 2,
      ts_builtin_sym_end,
      sym_variable_name,
  [409] = 1,
    ACTIONS(85), 2,
      ts_builtin_sym_end,
      sym_variable_name,
  [414] = 1,
    ACTIONS(87), 2,
      ts_builtin_sym_end,
      sym_variable_name,
  [419] = 1,
    ACTIONS(89), 1,
      ts_builtin_sym_end,
  [423] = 1,
    ACTIONS(91), 1,
      anon_sym_RPAREN,
  [427] = 1,
    ACTIONS(93), 1,
      anon_sym_EQ,
  [431] = 1,
    ACTIONS(95), 1,
      anon_sym_DASH_GT,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(10)] = 0,
  [SMALL_STATE(11)] = 25,
  [SMALL_STATE(12)] = 50,
  [SMALL_STATE(13)] = 75,
  [SMALL_STATE(14)] = 100,
  [SMALL_STATE(15)] = 125,
  [SMALL_STATE(16)] = 150,
  [SMALL_STATE(17)] = 175,
  [SMALL_STATE(18)] = 203,
  [SMALL_STATE(19)] = 226,
  [SMALL_STATE(20)] = 249,
  [SMALL_STATE(21)] = 271,
  [SMALL_STATE(22)] = 282,
  [SMALL_STATE(23)] = 293,
  [SMALL_STATE(24)] = 304,
  [SMALL_STATE(25)] = 315,
  [SMALL_STATE(26)] = 333,
  [SMALL_STATE(27)] = 342,
  [SMALL_STATE(28)] = 357,
  [SMALL_STATE(29)] = 366,
  [SMALL_STATE(30)] = 375,
  [SMALL_STATE(31)] = 390,
  [SMALL_STATE(32)] = 399,
  [SMALL_STATE(33)] = 404,
  [SMALL_STATE(34)] = 409,
  [SMALL_STATE(35)] = 414,
  [SMALL_STATE(36)] = 419,
  [SMALL_STATE(37)] = 423,
  [SMALL_STATE(38)] = 427,
  [SMALL_STATE(39)] = 431,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0, 0, 0),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [7] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expression_function_application, 2, 0, 2),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [11] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expression_function_type, 3, 0, 3),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [17] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [19] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [21] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [23] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expression_annotation, 3, 0, 3),
  [25] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expression_function_abstraction, 4, 0, 5),
  [27] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [29] = {.entry = {.count = 1, .reusable = true}}, SHIFT(35),
  [31] = {.entry = {.count = 1, .reusable = true}}, SHIFT(32),
  [33] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [35] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [37] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [39] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [41] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [43] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [45] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_pattern_list, 1, 0, 0),
  [47] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_pattern_list_repeat1, 2, 0, 0),
  [49] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_pattern_list_repeat1, 2, 0, 0), SHIFT_REPEAT(25),
  [52] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_pattern_list_repeat1, 2, 0, 0), SHIFT_REPEAT(29),
  [55] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_pattern_list_repeat1, 2, 0, 0), SHIFT_REPEAT(31),
  [58] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expression_constructor, 1, 0, 0),
  [60] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expression_variable, 1, 0, 0),
  [62] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expression, 1, 0, 0),
  [64] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expression_parentheses, 3, 0, 0),
  [66] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_pattern, 1, 0, 0),
  [68] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0),
  [70] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(17),
  [73] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_pattern_parentheses, 3, 0, 0),
  [75] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_pattern_variable, 1, 0, 0),
  [77] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1, 0, 0),
  [79] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_pattern_constructor, 1, 0, 0),
  [81] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_declaration_type_signature, 4, 0, 1),
  [83] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_declaration, 1, 0, 0),
  [85] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_declaration_function, 4, 0, 1),
  [87] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_declaration_function, 5, 0, 4),
  [89] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [91] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [93] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [95] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
};

#ifdef __cplusplus
extern "C" {
#endif
#ifdef TREE_SITTER_HIDE_SYMBOLS
#define TS_PUBLIC
#elif defined(_WIN32)
#define TS_PUBLIC __declspec(dllexport)
#else
#define TS_PUBLIC __attribute__((visibility("default")))
#endif

TS_PUBLIC const TSLanguage *tree_sitter_gilear(void) {
  static const TSLanguage language = {
    .version = LANGUAGE_VERSION,
    .symbol_count = SYMBOL_COUNT,
    .alias_count = ALIAS_COUNT,
    .token_count = TOKEN_COUNT,
    .external_token_count = EXTERNAL_TOKEN_COUNT,
    .state_count = STATE_COUNT,
    .large_state_count = LARGE_STATE_COUNT,
    .production_id_count = PRODUCTION_ID_COUNT,
    .field_count = FIELD_COUNT,
    .max_alias_sequence_length = MAX_ALIAS_SEQUENCE_LENGTH,
    .parse_table = &ts_parse_table[0][0],
    .small_parse_table = ts_small_parse_table,
    .small_parse_table_map = ts_small_parse_table_map,
    .parse_actions = ts_parse_actions,
    .symbol_names = ts_symbol_names,
    .field_names = ts_field_names,
    .field_map_slices = ts_field_map_slices,
    .field_map_entries = ts_field_map_entries,
    .symbol_metadata = ts_symbol_metadata,
    .public_symbol_map = ts_symbol_map,
    .alias_map = ts_non_terminal_alias_map,
    .alias_sequences = &ts_alias_sequences[0][0],
    .lex_modes = ts_lex_modes,
    .lex_fn = ts_lex,
    .primary_state_ids = ts_primary_state_ids,
  };
  return &language;
}
#ifdef __cplusplus
}
#endif
