package com.nigeleke.macro11.parser

import com.nigeleke.macro11.ast.*

import scala.util.parsing.combinator.*

trait OperandParser extends UtilityParser with RegexParsers:

  private def register =
    val names         = (0 to 7).map(r => s"%$r") ++ (0 to 5).map(r => s"R$r") ++ Seq("SP", "PC")
    val first :: rest = names.map(_ ^^ { identity }).toList: @unchecked
    rest.foldLeft(first)((rs, r) => { rs ||| r })

  private def registerMode =
    register ^^ { Operand.RegisterMode.apply }

  private def registerDeferredMode =
    (("@" ~> register) | ("(" ~> register <~ ")")) ^^ { Operand.RegisterDeferredMode.apply }

  private def autoIncrementMode =
    "(" ~> register <~ ")+" ^^ { Operand.AutoIncrementMode.apply }

  private def autoIncrementDeferredMode =
    "@(" ~> register <~ ")+" ^^ { Operand.AutoIncrementDeferredMode.apply }

  private def autoDecrementMode =
    "-(" ~> register <~ ")" ^^ { Operand.AutoDecrementMode.apply }

  private def autoDecrementDeferredMode =
    "@-(" ~> register <~ ")" ^^ { Operand.AutoDecrementDeferredMode.apply }

  private def indexMode =
    symbol ~ ("(" ~> register <~ ")") ^^ { case s ~ r => Operand.IndexMode(s, r) }

  private def indexDeferredMode =
    "@" ~> symbol ~ ("(" ~> register <~ ")") ^^ { case s ~ r => Operand.IndexDeferredMode(s, r) }

  private def immediateMode =
    "#" ~> symbol ^^ { Operand.ImmediateMode.apply }

  private def absoluteMode =
    "@#" ~> symbol ^^ { Operand.AbsoluteMode.apply }

  private def relativeMode =
    symbol ^^ { Operand.RelativeMode.apply }

  private def relativeDeferredMode =
    "@" ~> symbol ^^ { Operand.RelativeDeferredMode.apply }

  def addressingModeOperand: Parser[Operand.AddressingModeOperand] =
    registerMode ||| registerDeferredMode |||
      autoIncrementMode ||| autoIncrementDeferredMode |||
      autoDecrementMode ||| autoDecrementDeferredMode |||
      indexMode ||| indexDeferredMode |||
      immediateMode ||| absoluteMode |||
      relativeMode ||| relativeDeferredMode

  def addressOffsetOperand: Parser[Operand.AddressOffsetOperand] =
    symbol ^^ { Operand.AddressOffsetOperand.apply }

  def registerOperand: Parser[Operand.RegisterOperand] =
    register ^^ { Operand.RegisterOperand.apply }

  def parameterCountOperand: Parser[Operand.ParameterCountOperand] =
    symbol ^^ { Operand.ParameterCountOperand.apply }

  def trapParameterOperand: Parser[Operand.TrapParameterOperand] =
    symbol ^^ { Operand.TrapParameterOperand.apply }
