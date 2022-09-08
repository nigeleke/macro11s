package com.nigeleke.macro11.ast

// sealed trait Instruction(val mnemonic: Instruction.Mnemonic)
// case class SingleOperandInstruction(override val mnemonic: Instruction.Mnemonic, operand: AddressingMode)
//     extends Instruction(mnemonic)
// case class DoubleOperandInstruction(
//     override val mnemonic: Instruction.Mnemonic,
//     source: AddressingMode,
//     destination: AddressingMode
// ) extends Instruction(mnemonic)
// case class BranchInstruction(override val mnemonic: Instruction.Mnemonic, address: String) extends Instruction(mnemonic)
// case class SobInstruction(register: String, address: String) extends Instruction(Instruction.Mnemonic.SOB)

case class Instruction(mnemonic: Instruction.Mnemonic, operands: Seq[Operand])

object Instruction:
  /** The `Instruction.Operand` defines types of operands used by `Instruction`s.
    */
  enum OperandRole:
    case Source, Destination, Register, AddressOffsetSigned, AddressOffsetUnsigned, ParameterCount, Trap

  enum Mnemonic(val operands: OperandRole*):
    // Single Operand Instructions...
    case CLR  extends Mnemonic(OperandRole.Destination)
    case CLRB extends Mnemonic(OperandRole.Destination)
    case DEC  extends Mnemonic(OperandRole.Destination)
    case DECB extends Mnemonic(OperandRole.Destination)
    case INC  extends Mnemonic(OperandRole.Destination)
    case INCB extends Mnemonic(OperandRole.Destination)
    case NEG  extends Mnemonic(OperandRole.Destination)
    case NEGB extends Mnemonic(OperandRole.Destination)
    case TST  extends Mnemonic(OperandRole.Destination)
    case TSTB extends Mnemonic(OperandRole.Destination)
    case COM  extends Mnemonic(OperandRole.Destination)
    case COMB extends Mnemonic(OperandRole.Destination)
    case ASR  extends Mnemonic(OperandRole.Destination)
    case ASRB extends Mnemonic(OperandRole.Destination)
    case ASL  extends Mnemonic(OperandRole.Destination)
    case ASLB extends Mnemonic(OperandRole.Destination)
    case ROL  extends Mnemonic(OperandRole.Destination)
    case ROLB extends Mnemonic(OperandRole.Destination)
    case ROR  extends Mnemonic(OperandRole.Destination)
    case RORB extends Mnemonic(OperandRole.Destination)
    case SWAB extends Mnemonic(OperandRole.Destination)
    case ADC  extends Mnemonic(OperandRole.Destination)
    case ADCB extends Mnemonic(OperandRole.Destination)
    case SBC  extends Mnemonic(OperandRole.Destination)
    case SBCB extends Mnemonic(OperandRole.Destination)
    case SXT  extends Mnemonic(OperandRole.Destination)
    // Double Operand Instructions...
    case MOV  extends Mnemonic(OperandRole.Source, OperandRole.Destination)
    case MOVB extends Mnemonic(OperandRole.Source, OperandRole.Destination)
    case ADD  extends Mnemonic(OperandRole.Source, OperandRole.Destination)
    case SUB  extends Mnemonic(OperandRole.Source, OperandRole.Destination)
    case CMP  extends Mnemonic(OperandRole.Source, OperandRole.Destination)
    case CMPB extends Mnemonic(OperandRole.Source, OperandRole.Destination)
    case BIS  extends Mnemonic(OperandRole.Source, OperandRole.Destination)
    case BISB extends Mnemonic(OperandRole.Source, OperandRole.Destination)
    case BIT  extends Mnemonic(OperandRole.Source, OperandRole.Destination)
    case BITB extends Mnemonic(OperandRole.Source, OperandRole.Destination)
    case BIC  extends Mnemonic(OperandRole.Source, OperandRole.Destination)
    case BICB extends Mnemonic(OperandRole.Source, OperandRole.Destination)
    // Program Control Instructions...
    case BR    extends Mnemonic(OperandRole.AddressOffsetSigned)
    case BEQ   extends Mnemonic(OperandRole.AddressOffsetSigned)
    case BNE   extends Mnemonic(OperandRole.AddressOffsetSigned)
    case BMI   extends Mnemonic(OperandRole.AddressOffsetSigned)
    case BPL   extends Mnemonic(OperandRole.AddressOffsetSigned)
    case BCS   extends Mnemonic(OperandRole.AddressOffsetSigned)
    case BCC   extends Mnemonic(OperandRole.AddressOffsetSigned)
    case BVS   extends Mnemonic(OperandRole.AddressOffsetSigned)
    case BVC   extends Mnemonic(OperandRole.AddressOffsetSigned)
    case BLT   extends Mnemonic(OperandRole.AddressOffsetSigned)
    case BGE   extends Mnemonic(OperandRole.AddressOffsetSigned)
    case BLE   extends Mnemonic(OperandRole.AddressOffsetSigned)
    case BGT   extends Mnemonic(OperandRole.AddressOffsetSigned)
    case BHI   extends Mnemonic(OperandRole.AddressOffsetSigned)
    case BLOS  extends Mnemonic(OperandRole.AddressOffsetSigned)
    case BLO   extends Mnemonic(OperandRole.AddressOffsetSigned)
    case BHIS  extends Mnemonic(OperandRole.AddressOffsetSigned)
    case SOB   extends Mnemonic(OperandRole.Register, OperandRole.AddressOffsetUnsigned)
    case JMP   extends Mnemonic(OperandRole.Destination)
    case JSR   extends Mnemonic(OperandRole.Register, OperandRole.Destination)
    case RTS   extends Mnemonic(OperandRole.Register)
    case MARK  extends Mnemonic(OperandRole.ParameterCount)
    case SPL   extends Mnemonic(OperandRole.Destination)
    case EMT   extends Mnemonic(OperandRole.Trap)
    case TRAP  extends Mnemonic(OperandRole.Trap)
    case BPT   extends Mnemonic
    case IOT   extends Mnemonic
    case RTI   extends Mnemonic
    case HALT  extends Mnemonic
    case WAIT  extends Mnemonic
    case RESET extends Mnemonic
    case MTPS  extends Mnemonic(OperandRole.Source)
    case MFPS  extends Mnemonic(OperandRole.Destination)
    case NOP   extends Mnemonic
    case CLN   extends Mnemonic
    case SEN   extends Mnemonic
    case CLZ   extends Mnemonic
    case SEZ   extends Mnemonic
    case CLV   extends Mnemonic
    case SEV   extends Mnemonic
    case CLC   extends Mnemonic
    case SEC   extends Mnemonic
    case CCC   extends Mnemonic
    case SCC   extends Mnemonic
    // Extended Instruction Set...
    case MUL  extends Mnemonic(OperandRole.Register, OperandRole.Source)
    case DIV  extends Mnemonic(OperandRole.Register, OperandRole.Source)
    case ASH  extends Mnemonic(OperandRole.Register, OperandRole.Source)
    case ASHC extends Mnemonic(OperandRole.Register, OperandRole.Source)
    case XOR  extends Mnemonic(OperandRole.Register, OperandRole.Destination)
    // Floating Instruction Set...
    case FADD extends Mnemonic(OperandRole.Register)
    case FSUB extends Mnemonic(OperandRole.Register)
    case FMUL extends Mnemonic(OperandRole.Register)
    case FDIV extends Mnemonic(OperandRole.Register)

    def fits(roles: OperandRole*) = roles.forall(operands.contains(_)) && roles.size == operands.size
