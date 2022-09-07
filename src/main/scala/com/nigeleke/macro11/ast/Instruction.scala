package com.nigeleke.macro11.ast

sealed trait Instruction(val mnemonic: Instruction.Mnemonic)
case class SingleOperandInstruction(override val mnemonic: Instruction.Mnemonic, operand: AddressingMode)
    extends Instruction(mnemonic)
case class DoubleOperandInstruction(
    override val mnemonic: Instruction.Mnemonic,
    source: AddressingMode,
    destination: AddressingMode
) extends Instruction(mnemonic)
case class BranchInstruction(override val mnemonic: Instruction.Mnemonic, address: String) extends Instruction(mnemonic)
case class SobInstruction(register: String, address: String) extends Instruction(Instruction.Mnemonic.SOB)

object Instruction:
  enum Mnemonic:
    case CLR, CLRB, COM, COMB, INC, INCB, DEC, DECB, NEG, NEGB, TST, TSTB, ASR, ASRB, ASL, ASLB, ROR, RORB, ROL, ROLB, SWAB, ADC,
      ADCB, SBC, SBCB, SXT, BIT, BITB, BIC, BICB, BIS, BISB, ADD, SUB, MOV, MOVB, CMP, CMPB, BR, BEQ, BNE, BMI, BPL, BCS, BCC, BVS,
      BVC, BGE, BLE, BGT, BHI, BLOS, BLO, BHIS, SOB

  // format: off
  val singleOperandMnemonics = Seq(
    Mnemonic.CLR,   Mnemonic.CLRB,    Mnemonic.COM,   Mnemonic.COMB,  Mnemonic.INC,   Mnemonic.INCB,
    Mnemonic.DEC,   Mnemonic.DECB,    Mnemonic.NEG,   Mnemonic.NEGB,  Mnemonic.TST,   Mnemonic.TSTB,
    Mnemonic.ASR,   Mnemonic.ASRB,    Mnemonic.ASL,   Mnemonic.ASLB,  Mnemonic.ROR,   Mnemonic.RORB,
    Mnemonic.ROL,   Mnemonic.ROLB,    Mnemonic.SWAB,  Mnemonic.ADC,   Mnemonic.ADCB,  Mnemonic.SBC, 
    Mnemonic.SBCB,  Mnemonic.SXT
  )
  val doubleOperandMnemonics = Seq(
    Mnemonic.BIT,   Mnemonic.BITB,    Mnemonic.BIC,   Mnemonic.BICB,  Mnemonic.BIS,   Mnemonic.BISB,
    Mnemonic.ADD,   Mnemonic.SUB,     Mnemonic.MOV,   Mnemonic.MOVB,  Mnemonic.CMP,   Mnemonic.CMPB
  )
  val branchMnemonics = Seq(
    Mnemonic.BR,    Mnemonic.BEQ,     Mnemonic.BNE,   Mnemonic.BMI,   Mnemonic.BPL,   Mnemonic.BCS,
    Mnemonic.BCC,   Mnemonic.BVS,     Mnemonic.BVC,   Mnemonic.BGE,   Mnemonic.BLE,   Mnemonic.BGT,
    Mnemonic.BHI,   Mnemonic.BLOS,    Mnemonic.BLO,   Mnemonic.BHIS
  )
  // format: on
