package com.nigeleke.macro11.parser

import com.nigeleke.macro11.ast.*

import scala.util.parsing.combinator.RegexParsers

trait UtilityParser extends RegexParsers:

  val anyCharacter = """.""".r ^^ { identity }
  val anyString    = """.*""".r ^^ { identity }
  val digits       = """\d+""".r ^^ { identity }
  val separator    = "," ^^ { identity } // <tab> & <sp> managed by scala parser combinator
  def comment      = opt(";.*".r).map(_.getOrElse("")) ^^ { Comment.apply }
  def symbol       = """[\\$\\.A-Za-z][\\$\\.A-Za-z0-9]*""".r ^^ { identity }

  def exact(needed: String) =
    symbol >> { parsed =>
      if parsed.equals(needed)
      then success(parsed)
      else err(s"Found $parsed, not $needed")
    }

  private def binaryOperator = "+" | "-" | "*" | "/" ^^ { identity }
  private def unaryOperator  = "+" | "-" ^^ { identity }
  private def binaryOperatorExpression = term ~ binaryOperator ~ term ^^ { case lhs ~ o ~ rhs =>
    BinaryOperatorExpression(lhs, o, rhs)
  }
  private def simpleExpression = term ^^ { SimpleExpression(_) }
  def expression               = binaryOperatorExpression ||| simpleExpression

//  def stringUntil(c: Char) = rep(elem("", _ != c)) ^^ { _.mkString("") }
  def macroArgument = expressionTerm

  private def ascii1Term                 = "'" ~> anyCharacter ^^ { AsciiChar1Term(_) }
  private def ascii2Term                 = "\"" ~> anyCharacter ~ anyCharacter ^^ { case c1 ~ c2 => AsciiChar2Term(s"$c1$c2") }
  private def currentLocationCounterTerm = exact(".") ^^ { _ => CurrentLocationCounterTerm }
  private def expressionTerm             = "<" ~> expression <~ ">" ^^ { ExpressionTerm(_) }
  private def numericTerm                = digits ^^ { NumericTerm(_) }
  private def symbolTerm                 = symbol ^^ { SymbolTerm(_) }
  private def unaryOperatorTerm = unaryOperator ~ (digits | symbol) ^^ { case operator ~ operand =>
    UnaryOperatorTerm(operator, operand)
  }

  def term: Parser[Term] =
    ascii1Term ||| ascii2Term |||
      currentLocationCounterTerm |||
      expressionTerm |||
      numericTerm |||
      symbolTerm |||
      unaryOperatorTerm
