package com.nigeleke.macro11.parser

import com.nigeleke.macro11.ast.*
import com.nigeleke.macro11.parser.*

import scala.util.parsing.combinator.*

trait Macro11Parser extends DirectiveParser with StatementParser with UtilityParser with RegexParsers:

  private def line = directive ||| statement
  def program: Parser[Program] = repsep(line, "\n") ^^ { Program.apply }
