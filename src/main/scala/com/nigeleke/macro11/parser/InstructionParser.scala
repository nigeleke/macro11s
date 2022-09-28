package com.nigeleke.macro11.parser

import com.nigeleke.macro11.ast.*
import com.nigeleke.macro11.ast.Instruction.Mnemonic
import com.nigeleke.macro11.ast.Instruction.OperandRole
import com.nigeleke.macro11.parser.*

import scala.util.parsing.combinator.*

trait InstructionParser extends OperandParser with RegexParsers:

  private def mnemonicForRoles(roles: Instruction.OperandRole*) =
    val first :: rest = Instruction.Mnemonic.values.toList
      .filter(_.requiring(roles: _*))
      .map(_.toString ^^ { identity }): @unchecked
    val parser = rest.foldLeft(first)((ms, m) => ms ||| m)
    parser ^^ { Instruction.Mnemonic.valueOf }

  private def noOperandInstruction =
    val validMnemonics = mnemonicForRoles()
    validMnemonics ^^ { i => Instruction(i, Seq.empty) }

  private def destinationOperandInstruction =
    val validMnemonics = mnemonicForRoles(OperandRole.Destination)
    validMnemonics ~ addressingModeOperand ^^ { case i ~ ddd => Instruction(i, Seq(ddd)) }

  private def sourceDestinationOperandInstruction =
    val validMnemonics = mnemonicForRoles(OperandRole.Source, OperandRole.Destination)
    validMnemonics ~ addressingModeOperand ~ ("," ~> addressingModeOperand) ^^ { case i ~ sss ~ ddd =>
      Instruction(i, Seq(sss, ddd))
    }

  private def addressOffsetInstruction =
    val validMnemonics = mnemonicForRoles(OperandRole.AddressOffsetSigned)
    validMnemonics ~ addressOffsetOperand ^^ { case i ~ a => Instruction(i, Seq(a)) }

  private def registerAddressOffsetInstruction =
    val validMnemonics = mnemonicForRoles(OperandRole.Register, OperandRole.AddressOffsetUnsigned)
    validMnemonics ~ registerOperand ~ ("," ~> addressOffsetOperand) ^^ { case i ~ r ~ a => Instruction(i, Seq(r, a)) }

  private def registerDestinationInstruction =
    val validMnemonics = mnemonicForRoles(OperandRole.Register, OperandRole.Destination)
    validMnemonics ~ registerOperand ~ ("," ~> addressingModeOperand) ^^ { case i ~ r ~ ddd => Instruction(i, Seq(r, ddd)) }

  private def parameterCountInstruction =
    val validMnemonics = mnemonicForRoles(OperandRole.ParameterCount)
    validMnemonics ~ parameterCountOperand ^^ { case i ~ pc => Instruction(i, Seq(pc)) }

  private def trapInstruction =
    val validMnemonics = mnemonicForRoles(OperandRole.Trap)
    validMnemonics ~ trapParameterOperand ^^ { case i ~ tp => Instruction(i, Seq(tp)) }

  private def registerSourceInstruction =
    val validMnemonics = mnemonicForRoles(OperandRole.Register, OperandRole.Source)
    validMnemonics ~ registerOperand ~ ("," ~> addressingModeOperand) ^^ { case i ~ r ~ sss => Instruction(i, Seq(r, sss)) }

  private def registerInstruction =
    val validMnemonics = mnemonicForRoles(OperandRole.Register)
    validMnemonics ~ registerOperand ^^ { case i ~ r => Instruction(i, Seq(r)) }

  def instruction: Parser[Instruction] =
    noOperandInstruction ||| destinationOperandInstruction |||
      sourceDestinationOperandInstruction ||| addressOffsetInstruction |||
      registerAddressOffsetInstruction ||| registerDestinationInstruction |||
      parameterCountInstruction ||| trapInstruction |||
      registerSourceInstruction ||| registerInstruction
