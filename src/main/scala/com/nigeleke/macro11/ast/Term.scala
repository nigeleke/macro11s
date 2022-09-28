package com.nigeleke.macro11.ast

/** A [[Term]] is a component of an expression and may be one of the following:
  *   1. A number whose 16-bit value is used.
  *   1. A symbol evaluated as follows:
  *      1. A period (.) specified in an expression causes the value of the current location counter to be used.
  *      1. A defined symbol is located in the User-Defined Symbol Table (UST) and its value is used.
  *      1. A permanent symbol's basic value is used, with zero substituted for the addressing modes.
  *      1. An undefined symbol is assugned a value of zero and inserted into the User-Defined Symbol Table as an undefined default
  *         global reference. If the `DSABL GBL` directive is in effect, the automatic global reference default function of MACRO-11
  *         is inhibited, and the statement containing the undefined symbol is flagged with an error.
  *   1. A single quote followed by a single ASCII character, or a double quote followed by two ASCII characters.
  *   1. An expression enclosed in angle brackets (<>) . Any expression so enclosed is evaluated and reduced to a single term before
  *      the remainder of the expression in which it appears is evaluated. Angle brackets, for example, may be used to alter the
  *      left-to-right evaluation of expressions (as in A*B+C versus A*<B+C>), or to apply a unary operator to an entire expression
  *      (as in -<A+B>).
  *   1. A unary operator followed by a symbol or number.
  */
trait Term

/** The single quoted ASCII character(s) term.
  * @param char
  *   The character following the '.
  */
final case class AsciiChar1Term(char: String) extends Term

/** The double quoted ASCII character(s) term.
  * @param char
  *   The two characters following the ".
  */
final case class AsciiChar2Term(char: String) extends Term

/** The '.' current location counter term. */
case object CurrentLocationCounterTerm extends Term

/** @todo
  * @param expression
  */
final case class ExpressionTerm(expression: Expression) extends Term

/** A simple numeric term,
  * @param digits
  *   The digits comprising the numeric term,
  */
final case class NumericTerm(digits: String) extends Term

/** A register term. A [[RegisterExpression]] must contain a [[RegisterTerm]].
  * @param register
  *   The register.
  */
final case class RegisterTerm(register: String) extends Term

/** A symbol term.
  * @param symbol
  *   The symbol for the user defined symbol table.
  */
final case class SymbolTerm(symbol: String) extends Term

/** A Unary operator term.
  * @param operator
  *   The unary operator.
  * @param operand
  *   The operand on which the operator applies.
  */
final case class UnaryOperatorTerm(operator: String, operand: String) extends Term
