package com.nigeleke.macro11.ast

trait Term

case class AsciiChar1Term(char: String)                        extends Term
case class AsciiChar2Term(char: String)                        extends Term
case object CurrentLocationCounterTerm                         extends Term
case class ExpressionTerm(expression: Expression)              extends Term
case class NumericTerm(digits: String)                         extends Term
case class SymbolTerm(symbol: String)                          extends Term
case class UnaryOperatorTerm(operator: String, operand: String) extends Term
