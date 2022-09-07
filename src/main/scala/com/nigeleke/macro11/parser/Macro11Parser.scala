package com.nigeleke.macro11.parser

import com.nigeleke.macro11.ast.*

import scala.util.parsing.combinator.*

class Macro11Parser extends RegexParsers:
  def symbol: Parser[String] = """[\\$\\.A-Za-z0-9]*""".r ^^ { identity }

  private val register =
    val names         = (0 to 7).map(r => s"%$r") ++ (0 to 5).map(r => s"R$r") ++ Seq("SP", "PC")
    val first :: rest = names.map(_ ^^ { identity }).toList
    rest.foldLeft(first)((rs, r) => { rs ||| r })

  // TODO: Expand expressions...
  private val registerExpression = register

  // TODO: Expand expressions...
  private val expression = symbol

  private val address = symbol

  def globalLabel: Parser[GlobalLabel] = (symbol <~ "::") ^^ { GlobalLabel.apply }
  def localLabel: Parser[LocalLabel]   = (symbol <~ ":") ^^ { LocalLabel.apply }
  def label: Parser[Label]             = globalLabel ||| localLabel
  def labels: Parser[List[Label]]      = rep(label)

  def comment: Parser[Comment] = """;.*""".r ^^ { Comment.apply }

  // format: off
  def registerMode: Parser[AddressingMode] = register ^^ { RegisterMode.apply }
  def registerDeferredMode: Parser[AddressingMode] = (("@" ~> register) | ("(" ~> registerExpression <~ ")")) ^^ { RegisterDeferredMode.apply }
  def autoIncrementMode: Parser[AddressingMode] = ("(" ~> registerExpression <~ ")+") ^^ { AutoIncrementMode.apply }
  def autoIncrementDeferredMode: Parser[AddressingMode] = ("@(" ~> registerExpression <~ ")+") ^^ { AutoIncrementDeferredMode.apply }
  def autoDecrementMode: Parser[AddressingMode] = ("-(" ~> registerExpression <~ ")") ^^ { AutoDecrementMode.apply }
  def autoDecrementDeferredMode: Parser[AddressingMode] = ("@-(" ~> registerExpression <~ ")") ^^ { AutoDecrementDeferredMode.apply }
  def indexMode: Parser[AddressingMode] = (expression ~ ("(" ~> registerExpression <~ ")")) ^^ { case e ~ re => IndexMode(e, re)}
  def indexDeferredMode: Parser[AddressingMode] = ("@" ~> expression ~ ("(" ~> registerExpression <~ ")")) ^^ { case e ~ re => IndexDeferredMode(e, re) }
  def immediateMode: Parser[AddressingMode] = ("#" ~> expression) ^^ { ImmediateMode.apply }
  def absoluteMode: Parser[AddressingMode] = ("@#" ~> expression) ^^ { AbsoluteMode.apply }
  def relativeMode: Parser[AddressingMode] = expression ^^ { RelativeMode.apply }
  def relativeDeferredMode: Parser[AddressingMode] = ("@" ~> expression) ^^ { RelativeDeferredMode.apply }
  def branch: Parser[AddressingMode] = address ^^ { Branch.apply }
  // format: on

  def addressingModeOperand: Parser[AddressingMode] =
    registerMode ||| registerDeferredMode |||
      autoIncrementMode ||| autoIncrementDeferredMode |||
      autoDecrementMode ||| autoDecrementDeferredMode |||
      indexMode ||| indexDeferredMode |||
      immediateMode ||| absoluteMode |||
      relativeMode ||| relativeDeferredMode |||
      branch

  def singleOperandInstruction: Parser[SingleOperandInstruction] =
    val first :: rest = Instruction.singleOperandMnemonics.map(_.toString ^^ { identity }): @unchecked
    val instructions  = rest.foldLeft(first)((is, i) => { is ||| i })
    (instructions) ~ addressingModeOperand ^^ { case i ~ o => SingleOperandInstruction(Instruction.Mnemonic.valueOf(i), o) }

  def doubleOperandInstruction: Parser[DoubleOperandInstruction] =
    val first :: rest = Instruction.doubleOperandMnemonics.map(_.toString ^^ { identity }): @unchecked
    val instructions  = rest.foldLeft(first)((is, i) => { is ||| i })
    (instructions) ~ addressingModeOperand ~ ("," ~> addressingModeOperand) ^^ { case i ~ s ~ d =>
      DoubleOperandInstruction(Instruction.Mnemonic.valueOf(i), s, d)
    }

  def branchInstruction: Parser[BranchInstruction] =
    val first :: rest = Instruction.branchMnemonics.map(_.toString ^^ { identity }): @unchecked
    val instructions  = rest.foldLeft(first)((is, i) => { is ||| i })
    (instructions) ~ symbol ^^ { case i ~ s =>
      BranchInstruction(Instruction.Mnemonic.valueOf(i), s)
    }

  def sobInstruction: Parser[SobInstruction] =
    Instruction.Mnemonic.SOB.toString ~> register ~ ("," ~> symbol) ^^ { case r ~ s =>
      SobInstruction(r, s)
    }

  def instruction: Parser[Instruction] = singleOperandInstruction | doubleOperandInstruction | branchInstruction | sobInstruction

  def instructionLine: Parser[InstructionLine] =
    (labels ~ opt(instruction) ~ opt(comment)) ^^ { case ls ~ i ~ c => InstructionLine(ls, i, c) }

  def directive: Parser[Directive]         = ???
  def directiveLine: Parser[DirectiveLine] = directive ^^ { DirectiveLine.apply }

  def statementLine: Parser[StatementLine] = instructionLine ||| directiveLine
  def program: Parser[Program]             = rep(statementLine) ^^ { Program.apply }

object Macro11Parser extends Macro11Parser:
  def main(args: Array[String]) =
    val result = Macro11Parser.parse(instruction, "DECB R0")
    println(result)
