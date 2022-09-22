package com.nigeleke.macro11.ast

trait Expression
case class BinaryOperatorExpression(lhs: Term, op: String, rhs: Term) extends Expression
case class SimpleExpression(term: Term)                               extends Expression
