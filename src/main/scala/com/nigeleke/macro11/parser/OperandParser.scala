package com.nigeleke.macro11.parser

import com.nigeleke.macro11.ast.*

import scala.util.parsing.combinator.*

trait OperandParser extends UtilityParser with RegexParsers:

  private def registerMode              = registerExpression ^^ { AddressingMode.Register.apply }
  private def registerDeferredMode1     = "@" ~> registerExpression ^^ { AddressingMode.RegisterDeferred.apply }
  private def registerDeferredMode2     = "(" ~> registerExpression <~ ")" ^^ { AddressingMode.RegisterDeferred.apply }
  private def registerDeferredMode      = registerDeferredMode1 | registerDeferredMode2
  private def autoIncrementMode         = "(" ~> registerExpression <~ ")+" ^^ { AddressingMode.AutoIncrement.apply }
  private def autoIncrementDeferredMode = "@(" ~> registerExpression <~ ")+" ^^ { AddressingMode.AutoIncrementDeferred.apply }
  private def autoDecrementMode         = "-(" ~> registerExpression <~ ")" ^^ { AddressingMode.AutoDecrement.apply }
  private def autoDecrementDeferredMode = "@-(" ~> registerExpression <~ ")" ^^ { AddressingMode.AutoDecrementDeferred.apply }
  private def indexMode = expression ~ ("(" ~> registerExpression <~ ")") ^^ { case xe ~ re =>
    AddressingMode.Index(xe, re)
  }
  private def indexDeferredMode = "@" ~> expression ~ ("(" ~> registerExpression <~ ")") ^^ { case xe ~ re =>
    AddressingMode.IndexDeferred(xe, re)
  }
  private def immediateMode        = "#" ~> expression ^^ { AddressingMode.Immediate.apply }
  private def absoluteMode         = "@#" ~> expression ^^ { AddressingMode.Absolute.apply }
  private def relativeMode         = expression ^^ { AddressingMode.Relative.apply }
  private def relativeDeferredMode = "@" ~> expression ^^ { AddressingMode.RelativeDeferred.apply }

  def addressingModeOperand: Parser[Operand.AddressingModeOperand] =
    (registerMode ||| registerDeferredMode |||
      autoIncrementMode ||| autoIncrementDeferredMode |||
      autoDecrementMode ||| autoDecrementDeferredMode |||
      indexMode ||| indexDeferredMode |||
      immediateMode ||| absoluteMode |||
      relativeMode ||| relativeDeferredMode) ^^ { Operand.AddressingModeOperand.apply }

  def addressOffsetOperand: Parser[Operand.AddressOffsetOperand] =
    symbol ^^ { Operand.AddressOffsetOperand.apply }

  def registerOperand: Parser[Operand.RegisterOperand] =
    register ^^ { Operand.RegisterOperand.apply }

  def parameterCountOperand: Parser[Operand.ParameterCountOperand] =
    symbol ^^ { Operand.ParameterCountOperand.apply }

  def trapParameterOperand: Parser[Operand.TrapParameterOperand] =
    symbol ^^ { Operand.TrapParameterOperand.apply }
