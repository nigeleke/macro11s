package com.nigeleke.macro11.parser

import com.nigeleke.macro11.ast.*

import scala.util.parsing.combinator.RegexParsers

trait UtilityParser extends RegexParsers:

  val anyCharacter = """.""".r ^^ { identity }
  val anyString    = """.*""".r ^^ { identity }
  val separator    = "," ^^ { identity } // <tab> & <sp> managed by scala parser combinator
  def comment      = ";.*".r ^^ { Comment.apply }
  def symbol       = """[\\$\\.A-Za-z0-9]+""".r ^^ { identity }

  // TODO: Expand
  def expression        = symbol
  def numericExpression = symbol

  def stringUntil(c: Char) = rep(elem("", _ != c)) ^^ { _.mkString("") }
  def macroArgument        = "<" ~> stringUntil('>') <~ ">" ^^ { identity }
