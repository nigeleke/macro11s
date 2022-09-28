package com.nigeleke.macro11.parser

import com.nigeleke.macro11.ast.*

import scala.util.parsing.combinator.RegexParsers

trait UtilityParser extends RegexParsers:

  val anyCharacter: Parser[String] = """.""".r ^^ { identity }
  val anyString: Parser[String]    = """.*""".r ^^ { identity }
  val digits: Parser[String]       = """\d+""".r ^^ { identity }
  val separator: Parser[String]    = "," ^^ { identity } // <tab> & <sp> managed by scala parser combinator
  def comment: Parser[Comment]     = opt(";.*".r).map(_.getOrElse("")) ^^ { Comment.apply }
  def symbol: Parser[String]       = """[\\$\\.A-Za-z][\\$\\.A-Za-z0-9]*""".r ^^ { identity }

  def exact(needed: String): Parser[String] =
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
  private def simpleExpression       = term ^^ { SimpleExpression.apply }
  def expression: Parser[Expression] = binaryOperatorExpression ||| simpleExpression
  def registerExpression             = registerTerm ^^ { RegisterExpression.apply }

//  def stringUntil(c: Char) = rep(elem("", _ != c)) ^^ { _.mkString("") }
  def macroArgument: Parser[ExpressionTerm] = expressionTerm

  private def ascii1Term                 = "'" ~> anyCharacter ^^ { AsciiChar1Term.apply }
  private def ascii2Term                 = "\"" ~> anyCharacter ~ anyCharacter ^^ { case c1 ~ c2 => AsciiChar2Term(s"$c1$c2") }
  private def currentLocationCounterTerm = exact(".") ^^ { _ => CurrentLocationCounterTerm }
  private def expressionTerm             = "<" ~> expression <~ ">" ^^ { ExpressionTerm.apply }
  private def numericTerm                = digits ^^ { NumericTerm.apply }
  private def symbolTerm                 = symbol ^^ { SymbolTerm.apply }
  private def unaryOperatorTerm = unaryOperator ~ (digits | symbol) ^^ { case operator ~ operand =>
    UnaryOperatorTerm(operator, operand)
  }
  def register =
    val names = (0 to 7).map(r => s"%$r") ++ (0 to 5).map(r => s"R$r") ++ Seq("SP", "PC")
    val first :: rest = names
      .map(_ ^^ { identity })
      .toList: @unchecked
    rest.foldLeft(first)((rs, r) => { rs ||| r })
  def registerTerm = register ^^ { RegisterTerm.apply }

  def term: Parser[Term] =
    ascii1Term ||| ascii2Term |||
      currentLocationCounterTerm |||
      expressionTerm |||
      numericTerm |||
      registerTerm |||
      symbolTerm |||
      unaryOperatorTerm
