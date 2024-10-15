/**
 * @file Parser for the WHILE programming language.
 * @author Wen Kokke <wenkokke@users.noreply.github.com>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "while",

  precedences: (_) => [
    [
      "multiplication",
      "addition",
      "negation",
      "conjunction",
      "disjunction",
      "sequence",
    ],
  ],

  rules: {
    source_file: ($) => $.statements,

    statements: ($) => $._statements,
    _statements: ($) => repeat1($.statement),

    statement: ($) => $._statement,
    _statement: ($) =>
      choice(
        $.assignment,
        $.skip,
        $.sequence,
        $.if_then_else,
        $.while_do,
        $._bracketed_statements,
      ),

    assignment: ($) => seq($.variable, ":=", $.expression),

    skip: (_) => "skip",

    sequence: ($) =>
      prec.right("sequence", seq($._statement, ";", $._statement)),

    if_then_else: ($) =>
      prec.right(
        "sequence",
        seq("if", $._predicate, "then", $._statement, "else", $._statement),
      ),

    while_do: ($) =>
      prec.right("sequence", seq("while", $._predicate, "do", $._statement)),

    _bracketed_statements: ($) => seq("{", $._statements, "}"),

    predicate: ($) => $._predicate,
    _predicate: ($) =>
      choice(
        $.true,
        $.false,
        $.negation,
        $.conjunction,
        $.disjunction,
        $.comparison,
        $._parenthesized_predicate,
      ),

    true: ($) => "true",

    false: ($) => "false",

    negation: ($) => prec("negation", seq("not", $._predicate)),

    conjunction: ($) =>
      prec.left(
        "conjunction",
        seq(field("left", $._predicate), "and", field("right", $._predicate)),
      ),

    disjunction: ($) =>
      prec.left(
        "disjunction",
        seq(field("left", $._predicate), "or", field("right", $._predicate)),
      ),

    comparison: ($) =>
      seq(
        field("left", $._expression),
        $._comparison_operator,
        field("right", $._expression),
      ),

    _comparison_operator: ($) => choice($.lt, $.le, $.eq, $.gt, $.ge),

    lt: (_) => "<",
    le: (_) => "<=",
    eq: (_) => "=",
    gt: (_) => ">",
    ge: (_) => ">=",

    _parenthesized_predicate: ($) => seq("(", $._predicate, ")"),

    expression: ($) => $._expression,
    _expression: ($) =>
      choice(
        $.variable,
        $.number,
        $.addition,
        $.subtraction,
        $.multiplication,
        $.division,
        $._parenthesized_expression,
      ),

    addition: ($) =>
      prec.left(
        "addition",
        seq(field("left", $._expression), "+", field("right", $._expression)),
      ),

    subtraction: ($) =>
      prec.left(
        "addition",
        seq(field("left", $._expression), "-", field("right", $._expression)),
      ),

    multiplication: ($) =>
      prec.left(
        "multiplication",
        seq(field("left", $._expression), "*", field("right", $._expression)),
      ),

    division: ($) =>
      prec.left(
        "multiplication",
        seq(field("left", $._expression), "/", field("right", $._expression)),
      ),

    _parenthesized_expression: ($) => seq("(", $._expression, ")"),

    number: (_) => /[0-9]+/,

    variable: (_) => /[a-z]+/,
  },
});
