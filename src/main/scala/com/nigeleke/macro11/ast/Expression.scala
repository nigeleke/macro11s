package com.nigeleke.macro11.ast

/** [[Expression]]s are combinations of [[Term]]s joined together by binary operators. [[Expression]]s reduce to a 16-bit value. The
  * evaluation of an [[Expression]] includes the determination of its attributes. A resultant [[Expression]] value may be any one of
  * four types: relocatable, absolute, external, or complex relocatable.
  *
  * [[Expression]]s are evaluated from left to right with no operator hierarchy rules, except that unary operators take precedence
  * over binary operators. A [[Term]] preceded by a unary operator is considered to contain that operator. ([[Term]]s are evaluated,
  * where necessary, before their use in [[Expression]]s.)
  */
trait Expression
final case class BinaryOperatorExpression(lhs: Term, op: String, rhs: Term) extends Expression
final case class LeafExpression(term: Term)                                 extends Expression
