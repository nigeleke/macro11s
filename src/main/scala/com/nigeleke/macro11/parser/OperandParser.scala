package com.nigeleke.macro11.parser

import com.nigeleke.macro11.ast.*

import scala.util.parsing.combinator.*

trait OperandParser extends UtilityParser with RegexParsers:

  private def registerMode              = expression ^^ { AddressingMode.Register.apply }
  private def registerDeferredMode1     = "@" ~> expression ^^ { AddressingMode.RegisterDeferred.apply }
  private def registerDeferredMode2     = "(" ~> expression <~ ")" ^^ { AddressingMode.RegisterDeferred.apply }
  private def registerDeferredMode      = registerDeferredMode1 | registerDeferredMode2
  private def autoIncrementMode         = "(" ~> expression <~ ")+" ^^ { AddressingMode.AutoIncrement.apply }
  private def autoIncrementDeferredMode = "@(" ~> expression <~ ")+" ^^ { AddressingMode.AutoIncrementDeferred.apply }
  private def autoDecrementMode         = "-(" ~> expression <~ ")" ^^ { AddressingMode.AutoDecrement.apply }
  private def autoDecrementDeferredMode = "@-(" ~> expression <~ ")" ^^ { AddressingMode.AutoDecrementDeferred.apply }
  private def indexMode = expression ~ ("(" ~> expression <~ ")") ^^ { case xe ~ re =>
    AddressingMode.Index(xe, re)
  }
  private def indexDeferredMode = "@" ~> expression ~ ("(" ~> expression <~ ")") ^^ { case xe ~ re =>
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
    expression ^^ { Operand.AddressOffsetOperand.apply }

  def registerOperand: Parser[Operand.RegisterOperand] =
    expression ^^ { Operand.RegisterOperand.apply }

  def parameterCountOperand: Parser[Operand.ParameterCountOperand] =
    expression ^^ { Operand.ParameterCountOperand.apply }

  def trapParameterOperand: Parser[Operand.TrapParameterOperand] =
    expression ^^ { Operand.TrapParameterOperand.apply }
