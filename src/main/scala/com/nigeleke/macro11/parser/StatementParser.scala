package com.nigeleke.macro11.parser

import com.nigeleke.macro11.ast.*

import scala.util.parsing.combinator.*

trait StatementParser extends InstructionParser with RegexParsers:

  private def globalLabel: Parser[GlobalLabel] = (symbol <~ "::") ^^ { GlobalLabel.apply }
  private def localLabel: Parser[LocalLabel]   = (symbol <~ ":") ^^ { LocalLabel.apply }
  private def label: Parser[Label]             = globalLabel ||| localLabel

  private def labels: Parser[List[Label]] = rep(label)

  private def comment: Parser[Comment] = """;.*""".r ^^ { Comment.apply }

  def statement: Parser[Statement] =
    labels ~ opt(instruction) ~ opt(comment) ^^ { case ls ~ i ~ c =>
      Statement(ls, i, c)
    }
