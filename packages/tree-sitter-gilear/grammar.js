/**
 * @file Parser for the Gilear language
 * @author Wen Kokke <wenkokke@users.noreply.github.com>
 * @license NONE
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check
const reserved_symbols = {
  has_type: ":",
  definitional_equality: "=",
  function_arrow: "->",
  function_abstraction: "\\",
  function_end_of_pattern: ".",
  left_parenthesis: "(",
  right_parenthesis: ")",
  end_of_declaration: ";",
};

module.exports = grammar({
  name: "gilear",

  // externals: $ => [
  //   $.end_of_declaration,
  // ],

  supertypes: ($) => [$.declaration, $.pattern, $.expression],

  rules: {
    source_file: ($) => repeat($.declaration),

    // Declaration
    declaration: ($) =>
      choice($.declaration_type_signature, $.declaration_function),
    declaration_type_signature: ($) =>
      seq(
        field("name", $.variable_name),
        reserved_symbols.has_type,
        field("rhs", $.expression),
        reserved_symbols.end_of_declaration,
      ),
    declaration_function: ($) =>
      seq(
        field("name", $.variable_name),
        field("lhs", optional($.pattern_list)),
        reserved_symbols.definitional_equality,
        field("rhs", $.expression),
        reserved_symbols.end_of_declaration,
      ),

    // Pattern
    pattern_list: ($) => repeat1($.pattern),
    pattern: ($) =>
      choice($.pattern_variable, $.pattern_constructor, $.pattern_parentheses),
    pattern_variable: ($) => $.variable_name,
    pattern_constructor: ($) => $.constructor_name,
    pattern_parentheses: ($) =>
      seq(
        reserved_symbols.left_parenthesis,
        $.pattern,
        reserved_symbols.right_parenthesis,
      ),

    // Expression
    expression: ($) =>
      choice(
        $.expression_variable,
        $.expression_constructor,
        $.expression_function_type,
        $.expression_function_application,
        $.expression_function_abstraction,
        $.expression_annotation,
        $.expression_parentheses,
      ),
    expression_variable: ($) => $.variable_name,
    expression_constructor: ($) => $.constructor_name,
    expression_function_type: ($) =>
      prec.right(
        // precedence.expression_function_type,
        seq(
          field("lhs", $.expression),
          reserved_symbols.function_arrow,
          field("rhs", $.expression),
        ),
      ),
    expression_function_application: ($) =>
      prec.left(
        // precedence.expression_function_application,
        seq(field("fun", $.expression), field("arg", $.expression)),
      ),
    expression_function_abstraction: ($) =>
      prec.right(
        // precedence.expression_function_abstraction,
        seq(
          reserved_symbols.function_abstraction,
          field("lhs", $.pattern_list),
          reserved_symbols.function_arrow,
          field("rhs", $.expression),
        ),
      ),
    expression_annotation: ($) =>
      prec.right(
        // precedence.expression_annotation,
        seq(
          field("lhs", $.expression),
          reserved_symbols.has_type,
          field("rhs", $.expression),
        ),
      ),
    expression_parentheses: ($) =>
      seq(
        reserved_symbols.left_parenthesis,
        $.expression,
        reserved_symbols.right_parenthesis,
      ),

    // Name
    variable_name: ($) => /[a-z][A-Za-z0-9]*/,
    constructor_name: ($) => /[A-Z][A-Za-z0-9]*/,
  },
});
