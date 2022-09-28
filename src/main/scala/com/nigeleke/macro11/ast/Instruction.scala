package com.nigeleke.macro11.ast

/** The PDP-11 instruction set offers a wide selection of operations and addressing modes. To save memory space and to simplify the
  * implementation of control and communications applications, the PDP-11 instructions allow byte and word addressing in both
  * single- and double-operand formats. By using the double-operand instructions, you can perform several operations with a single
  * instruction. For example, ADD A,B adds the contents of location A to location B, storing the result in location B. Traditional
  * computers would implement this instruction this way:
  *
  * ``LDA A``
  *
  * ``ADD B``
  *
  * ``STR B``
  *
  * The PDP-11 instruction set also contains a full set of conditional branches that eliminate excessive use of jump instructions.
  * PDP-11 instructions fall into one of seven categoreis:
  *   1. Single-Operand - the first part of the word, called the "opcode", specifies the operation; the second part provides
  *      information for locating the operand.
  *   1. Double-Operand - the first part of the word specifies the operation to be performed; the remaining two parts provide
  *      information for location two operands.
  *   1. Branch - the first part of the word specifies the operation to be performed; the second part indicates where the action is
  *      to take place in the program.
  *   1. Jump and Subroutine - these instructions have an opcode and address part, and in the case of JSR, a register for linkage.
  *   1. Trap - these instructions contain an opcode only. In TRAP and EMT, the low-order byte may be used for function dispatching.
  *   1. Miscellaneous - HALT, WAIT, and Memory Management.
  *   1. Condition Code - these instructions set or clear the condition codes.
  *
  * @param mnemonic
  *   The Mnemonic (and hence required operands and instruction codes) of the [[Instruction()]].
  * @param operands
  *   A list of the actual operands matching the required ones.
  */
final case class Instruction(mnemonic: Instruction.Mnemonic, operands: Seq[Operand])

object Instruction:
  /** The `Instruction.Operand` defines types of operands used by `Instruction`s.
    */

  /** [[OperandRole]] specifies how the operand(s) of an Instruction will be used (and hence there allowed syntax).
    */
  enum OperandRole:
    case Source, Destination, Register, AddressOffsetSigned, AddressOffsetUnsigned, ParameterCount, Trap

  /** The [[Instruction]] mnemonic.
    * @param operandRoles
    *   A list of [[OperandRole]]s required by the instruction.
    */
  enum Mnemonic(val operandRoles: OperandRole*):
    /** Contents of the destination operand are replaced with zeroes.
      * @group Single Operand Instructions
      */
    case CLR extends Mnemonic(OperandRole.Destination)

    /** @see
      *   [[CLR]]
      * @group Single Operand Instructions
      */
    case CLRB extends Mnemonic(OperandRole.Destination)

    /** Subtract 1 from the contents of the destination operand.
      * @group Single Operand Instructions
      */
    case DEC extends Mnemonic(OperandRole.Destination)

    /** @see
      *   [[DEC]]
      * @group Single Operand Instructions
      */
    case DECB extends Mnemonic(OperandRole.Destination)

    /** Add 1 to the contents of the destination operand.
      * @group Single Operand Instructions
      */
    case INC extends Mnemonic(OperandRole.Destination)

    /** @see
      *   [[INC]]
      * @group Single Operand Instructions
      */
    case INCB extends Mnemonic(OperandRole.Destination)

    /** Replaces the contents of the destination operand by its two's complement. Note that word 100000 (or byte 200) is replaced by
      * itself (in two's complement notation the most negative number has no positive counterpart).
      * @group Single Operand Instructions
      */
    case NEG extends Mnemonic(OperandRole.Destination)

    /** @see
      *   [[NEG]]
      * @group Single Operand Instructions
      */
    case NEGB extends Mnemonic(OperandRole.Destination)

    /** Sets the condition codes N and Z according to the contents of the destination operand.
      * @group Single Operand Instructions
      */
    case TST extends Mnemonic(OperandRole.Destination)

    /** @see
      *   [[TST]]
      * @group Single Operand Instructions
      */
    case TSTB extends Mnemonic(OperandRole.Destination)

    /** Replaces the contents of the destination operand by their logical complement (each bit equal to 0 is set and each bit equal
      * to 1 is cleared).
      * @group Single Operand Instructions
      */
    case COM extends Mnemonic(OperandRole.Destination)

    /** @see
      *   [[COM]]
      * @group Single Operand Instructions
      */
    case COMB extends Mnemonic(OperandRole.Destination)

    /** Shifts all bits of the destination operand right one place. The MSB is replicated. The C-bit is loaded from the LSB of the
      * destination. ASR(B) performs signed division of the destination by two. Note: in the PDP-11/60, the ASRB does a
      * DATI/DATIP/DATO bus sequence in the execution portion of the instruction. This allows an interlocking of memory addresses.
      * If an I/O page reference is made, the ASRB does a DATIP/DATIP/DATO bus sequence during instruction execution.
      * @group Single Operand Instructions
      */
    case ASR extends Mnemonic(OperandRole.Destination)

    /** @see
      *   [[ASR]]
      * @group Single Operand Instructions
      */
    case ASRB extends Mnemonic(OperandRole.Destination)

    /** Shifts all bits of the destination operand left one place. The LSB is loaded with a 0. The C-bit of the status word is
      * loaded from the MSB of the destination. ASL(B) performs an equivalent signed multiplication of the destination by 2 with
      * overflow indication.
      * @group Single Operand Instructions
      */
    case ASL extends Mnemonic(OperandRole.Destination)

    /** @see
      *   [[ASL]]
      * @group Single Operand Instructions
      */
    case ASLB extends Mnemonic(OperandRole.Destination)

    /** Rotate all bits of the destination operand left one place. The MSB is loaded into the C-bit of the status word and the
      * previous contents of the C-bit are loaded into the LSB of the destination.
      * @group Single Operand Instructions
      */
    case ROL extends Mnemonic(OperandRole.Destination)

    /** @see
      *   [[ROL]]
      * @group Single Operand Instructions
      */
    case ROLB extends Mnemonic(OperandRole.Destination)

    /** Rotates all bits of the destination operand right one place. The LSB is loaded into the C-bit and the previous contents of
      * the C-bit are loaded into the MSB of the destination.
      * @group Single Operand Instructions
      */
    case ROR extends Mnemonic(OperandRole.Destination)

    /** @see
      *   [[ROR]]
      * @group Single Operand Instructions
      */
    case RORB extends Mnemonic(OperandRole.Destination)

    /** Exchanges high-order byte and low-order byte of the destination word.
      * @group Single Operand Instructions
      */
    case SWAB extends Mnemonic(OperandRole.Destination)

    /** Adds the contents of the C-bit into the destination operand. This permits the carry from the addition of the low-order words
      * to be carried into the high-order result. Note: byte operands cannot be used with ADD, so ADCB is not often used. The
      * example below has code that may be used to perform a double precision addition.
      * @group Single Operand Instructions
      */
    case ADC extends Mnemonic(OperandRole.Destination)

    /** @see
      *   [[ADC]]
      * @group Single Operand Instructions
      */
    case ADCB extends Mnemonic(OperandRole.Destination)

    /** Subtracts the contents of the C-bit from the destination. This permits the carry from the subtraction of two low-order words
      * to be subtracted from the high order part of the result. Note: byte operands cannot be used with SUB, so SBCB is not often
      * used. The example below illustrates code that may be used to perform a double precision subtraction.
      * @group Single Operand Instructions
      */
    case SBC extends Mnemonic(OperandRole.Destination)

    /** @see
      *   [[SBC]]
      * @group Single Operand Instructions
      */
    case SBCB extends Mnemonic(OperandRole.Destination)

    /** If the condition code bit N is set then a -1 is placed in the destination operand: if N bit is clear, then a 0 is placed in
      * the destination operand. This instruction is particularly useful in multiple precision arithmetic because it permits the
      * sign to be extended through multiple words.
      * @group Single Operand Instructions
      */
    case SXT extends Mnemonic(OperandRole.Destination)

    /** Moves the source operand to the destination operand. The previous contents of the destination are lost. The contents of the
      * source are not affected. Note: a MOVB to a register (unique among byte instructions) extends the most significant bit of the
      * low order byte (sign bit) into all bits of the high order byte (sign extension). Otherwise, MOVB operates on bytes exactly
      * as MOV operates on words.
      * @group Double Operand Instructions
      */
    case MOV extends Mnemonic(OperandRole.Source, OperandRole.Destination)

    /** @see
      *   [[MOV]]
      * @group Double Operand Instructions
      */
    case MOVB extends Mnemonic(OperandRole.Source, OperandRole.Destination)

    /** Adds the source operand to the destination operand and stores the result at the destination address. The original contents
      * of the destination are lost. The contents for the source are not affected. 16-bit binary addition is performed.
      * @group Double Operand Instructions
      */
    case ADD extends Mnemonic(OperandRole.Source, OperandRole.Destination)

    /** Subtracts the source operand from the destination operand and leaves the result at the destination address. The original
      * contents of the source are not affected. In double-precision arithmetic the C-bit, when set, indicates a "borrow".
      * @group Double Operand Instructions
      */
    case SUB extends Mnemonic(OperandRole.Source, OperandRole.Destination)

    /** Compares the source and destination operands and sets the condition codes, which may then be used for arithmetic and logical
      * conditional branches. Both operands are unaffected. The only action is to set the condition codes. The compare is
      * customarily followed by a conditional branch instruction. Note that opposite to the subtract instruction, the operation is
      * source - destination, not destination - source.
      * @group Double Operand Instructions
      */
    case CMP extends Mnemonic(OperandRole.Source, OperandRole.Destination)

    /** @see
      *   [[CMP]]
      * @group Double Operand Instructions
      */
    case CMPB extends Mnemonic(OperandRole.Source, OperandRole.Destination)

    /** Performs an "Inclusive OR" operation between the source and destination operands and leaves the result in the destination
      * operand; that is, correspoding bit positions as indicated in the source are set in the destination. The contents of the
      * destination are overwritten.
      * @group Double Operand Instructions
      */
    case BIS extends Mnemonic(OperandRole.Source, OperandRole.Destination)

    /** @see
      *   [[BIS]]
      * @group Double Operand Instructions
      */
    case BISB extends Mnemonic(OperandRole.Source, OperandRole.Destination)

    /** Performs a "Logical AND" operation between the source and destination operands and modifies condition codes accordingly.
      * Neither the source nor destination operands are affected. The BIT(B) instruction may be used to test whether any of the
      * corresponding bits that are set in the destination are also set in the source or whether all corresponding bits set in the
      * destination are clear in the source.
      * @group Double Operand Instructions
      */
    case BIT extends Mnemonic(OperandRole.Source, OperandRole.Destination)

    /** @see
      *   [[BIT]]
      * @group Double Operand Instructions
      */
    case BITB extends Mnemonic(OperandRole.Source, OperandRole.Destination)

    /** Clears each bit in the destination that corresponds to a set bit in the source. The original contents of the destination are
      * lost. The contents of the source are unaffected.
      * @group Double Operand Instructions
      */
    case BIC extends Mnemonic(OperandRole.Source, OperandRole.Destination)

    /** @see
      *   [[BIC]]
      * @group Double Operand Instructions
      */
    case BICB extends Mnemonic(OperandRole.Source, OperandRole.Destination)

    /** Provides a way of transferring program control within a range of -128 to +127 words with a one word instruction.
      * @group Program Control Instructions
      */
    case BR extends Mnemonic(OperandRole.AddressOffsetSigned)

    /** Tests the state of the Z-bit and causes a branch if Z is set. As an example, it is used to test equality following a CMP
      * operation, to test that no bits set in the destination were also set in the source following a BIT operation, and generally,
      * to test that the result of the previous operation was zero.
      * @group Program Control Instructions
      */
    case BEQ extends Mnemonic(OperandRole.AddressOffsetSigned)

    /** Tests the state of the Z-bit and causes a branch if the Z-bit is clear. BNE is the complementary operation to BEQ. It is
      * used to test inequality following a CMP, to test that some bits set in the destination were also in the source, following a
      * BIT, and generally, to test that the result of the previous operation was not zero.
      * @group Program Control Instructions
      */
    case BNE extends Mnemonic(OperandRole.AddressOffsetSigned)

    /** Tests the state of the N-bit and causes a branch if N is set. It is used to test the sign (most significant bit) of the
      * result of the previous operation, branching if negative.
      * @group Program Control Instructions
      */
    case BMI extends Mnemonic(OperandRole.AddressOffsetSigned)

    /** Tests the state of the N-bit and causes a branch if N is clear. BPL is the complementary operation of BMI.
      * @group Program Control Instructions
      */
    case BPL extends Mnemonic(OperandRole.AddressOffsetSigned)

    /** Tests the state of the C-bit and causes a branch if C is set. It is used to test for a carry in the result of a previous
      * operation.
      * @group Program Control Instructions
      */
    case BCS extends Mnemonic(OperandRole.AddressOffsetSigned)

    /** Tests the state of the C-bit and causes a branch if C is clear. BCC is the complementary operation to BCS.
      * @group Program Control Instructions
      */
    case BCC extends Mnemonic(OperandRole.AddressOffsetSigned)

    /** Tests the state of V bit (overflow) and causes a branch if the V bit is set. BVS is used to detect signed arithmetic
      * overflow in the previous operation.
      * @group Program Control Instructions
      */
    case BVS extends Mnemonic(OperandRole.AddressOffsetSigned)

    /** Tests the state of the V bit and causes a branch if the V bit is clear. BVC is complementary operation to BVS.
      * @group Program Control Instructions
      */
    case BVC extends Mnemonic(OperandRole.AddressOffsetSigned)

    /** Causes a branch if the "Exclusive Or" of the N and V bits are 1. Thus BLT will always branch following an operation that
      * added two negative numbers, even if overflow occurred. In particular, BLT will always cause a branch if it follows a CMP
      * instruction operating on a negative source and a positive destination (even if overflow occurred). Further, BLT will never
      * cause a branch when it follows a CMP instruction operating on a positive source and negative destination. BLT will not cause
      * a branch if the result of the previous operation was zero (without overflow).
      * @group Program Control Instructions
      */
    case BLT extends Mnemonic(OperandRole.AddressOffsetSigned)

    /** Causes a branch if N and V are either both clear or both set. BGE is the complementary operation to BLT. Thus BGE will
      * always cause a branch when it follows an operation that caused addition of two positive numbers. BGE will also cause a
      * branch on a zero result.
      * @group Program Control Instructions
      */
    case BGE extends Mnemonic(OperandRole.AddressOffsetSigned)

    /** Operation is similar to BLT but in addition will cause a branch if the resul of the previous operation was zero.
      * @group Program Control Instructions
      */
    case BLE extends Mnemonic(OperandRole.AddressOffsetSigned)

    /** Operation of BGT is similar to BGE, except BGT will not cause a branch on a zero result.
      * @group Program Control Instructions
      */
    case BGT extends Mnemonic(OperandRole.AddressOffsetSigned)

    /** Causes a branch if the previous operation caused neither a carry nor a zero result. This will happen in comparison (CMP)
      * operations as long as the source has a higher unsigned value than the destination.
      * @group Program Control Instructions
      */
    case BHI extends Mnemonic(OperandRole.AddressOffsetSigned)

    /** Causes a branch if the previous operation caused either a carry or a zero result. BLOS is the complementary operation to
      * BHI. The branch will occur in comparison operations as long as the source is equal to, or has a lower unsigned value than
      * the destination.
      * @group Program Control Instructions
      */
    case BLOS extends Mnemonic(OperandRole.AddressOffsetSigned)

    /** BLO is the same instruction as BCS. This mnemonic is included only for convenience, used instead of BCS when performing
      * unsigned comparisons, for documentation purposes.
      * @group Program Control Instructions
      */
    case BLO extends Mnemonic(OperandRole.AddressOffsetSigned)

    /** BHIS is the same instruction as BCC. This mnemonic is included only for convenience, used instead of BCC when performing
      * unsigned comparisons, for documentation purposes.
      * @group Program Control Instructions
      */
    case BHIS extends Mnemonic(OperandRole.AddressOffsetSigned)

    /** The register is decremented. If it is not equal to 0, twice the offset is subtracted from the PC (now pointing to the
      * following word). The offset is interpreted as a six bit unsigned (positive) number. This instruction provides a fast,
      * efficient method of loop control. Assembler syntax is: SOB R,A Where A is the address to which transfer is to be made if the
      * decremented R is not equal to 0. Note that the SOB instruction can not be used to transfer control in a forward direction.
      * @group Program Control Instructions
      * @note
      *   In the PDP-11/60, the SOB instruction may require more time than the sequence:
      *   ````
      *   DEC R
      *   BNE A
      *   ````
      */
    case SOB extends Mnemonic(OperandRole.Register, OperandRole.AddressOffsetUnsigned)

    /** JMP provides more flexible program branching than provided with the branch instructions. Control may be transferred to any
      * location in memory (no range limitation) and can be accomplished with the full flexibility of the addressing modes, with the
      * exception of register mode 0. Execution of a jump with mode 0 will cause an "illegal" instruciton condition and results in a
      * processor trap through the trap vector address 4 (program control cannot be transferred to the address of a register).
      * Register deferred mode is legal and will cause program control to be transferred to the address held in the specified
      * register. Note that instructions are word data and must therefore be fetched from an even-numbered address. A "boundary
      * error" trap condition will result when the processor attempts to fetch an instruction from an odd address. Deferred index
      * mode JMP instructions permit transfer of control to the address contained in a selectable element of a table of dispatch
      * vectors. This is commonly called a "jump table".
      * @group Program Control Instructions
      */
    case JMP extends Mnemonic(OperandRole.Destination)

    /** In execution of the JSR, the old contents of the specified register (the "Linkage Register") are automatically pushed onto
      * the processor stack and new linkage information placed in the register. Thus subroutines nested within subroutines to any
      * depth may all be called with the same linkage register. There is no need either to plan the maximum depth at which any
      * particular subroutine will be called or to include instructions in each routine to save and restore the linkage pointer.
      * Further, since all linkages are saved in a reentrant manner on the processor stack, execution of a subroutine reentered and
      * executed by an interrupt service routine. Execution of the initial subroutine can then be resumed when other requests are
      * satisfied. This process (called nesting) can proceed to any level.
      * @group Program Control Instructions
      */
    case JSR extends Mnemonic(OperandRole.Register, OperandRole.Destination)

    /** Loads the contents of reg into PC and pops the top element of the processor stack into the specified register. Return from a
      * non-reentrant subroutine is typically made through the same register that was used in its call. Thus, a subroutine called
      * with a JSR PC,dest exits with a RTS PC and a subroutine called with a JSR R5,dest may retrieve parameters with addressing
      * modes (R5)+, X(R5), or @X(R5), and finally exits with an RTS R5.
      * @group Program Control Instructions
      */
    case RTS extends Mnemonic(OperandRole.Register)

    /** Used as part of the standard PDP-11 subroutine return convention. MARK facilitates the stack clean up procedures involved in
      * subroutine exit. Assembler format is MARK N.
      * @group Program Control Instructions
      */
    case MARK extends Mnemonic(OperandRole.ParameterCount)

    /** SPL is not implemented in the PDP-11/60. A processor trap through vector address 10 occurs for illegal instruction.
      * @group Program Control Instructions
      */
    case SPL extends Mnemonic(OperandRole.Destination)

    /** All operation codes from 104000 to 104377 are EMT instructions and may be used to transmit information to the emulating
      * routine (e.g., function to be performed). The trap vector for EMT is at address 30. The new PC is taken from the word at
      * address 30; the new central processor status (PS) is taken from the word at address 32. Caution: EMT is used frequently by
      * DIGITAL system sofware and is therefore not recommended for general use.
      * @group Program Control Instructions
      */
    case EMT extends Mnemonic(OperandRole.Trap)

    /** Operation codes from 104400 to 104777 are TRAP instructions. TRAPs and EMTs are identical in operation, except that the trap
      * vector for TRAP is at address 34.
      * @group Program Control Instructions
      * @note
      *   Since DEC software makes frequent use of EMT, the TRAP instruction is recommended for general use. TRAPs are used to
      *   implement system calls in Unix Operating Systems (see Unix command "man 2 intro").
      */
    case TRAP extends Mnemonic(OperandRole.Trap)

    /** Performs a trap seqence with a trap vector address of 14. Used to call debugging aids. The user is cautioned against
      * employing code 000003 in programs run under these debugging aids (no information is transmitted in the low byte).
      * @group Program Control Instructions
      */
    case BPT extends Mnemonic

    /** Performs a trap sequence with a trap vector address of 20. Used to call the I/O Executive routine IOX in the paper tape
      * software system, and for error reporting in the Disk Operating System (no information is transmitted in the low byte).
      * @group Program Control Instructions
      */
    case IOT extends Mnemonic

    /** Used to exit from an interrupt or TRAP service routine. The PC and PS are restored (popped) from the processor stack.
      * @group Program Control Instructions
      */
    case RTI extends Mnemonic

    /** This is the same as the RTI instruction except that it inhibits a trace trap, while RTI permits a trace trap. If a trace
      * trap is pending, the first instruction after the RTT will be executed prior to the next "T" trap. In the case fo the RTI
      * instruction the "T" trap will occur immediately after the RTI.
      * @group Program Control Instructions
      */
    case RTT extends Mnemonic

    /** Causes the process operation to cease. The console is given control of the bus. The console data lights display the contents
      * of PC. The PC points to the next instruction to be executed. Pressing the continue key on the console causes processor
      * operation to resume. No INIT signal is given. Note: a HALT issued in User Mode will generate a trap through vector address
      * 10.
      * @group Program Control Instructions
      */
    case HALT extends Mnemonic

    /** Provides a way for the processor to relinquish use of the bus while it waits for an interrupt. Having been given a WAIT
      * command, the processor will not compete for bus use by fetching instructions or operands from memory. This permits higher
      * transfer rates between a device and memory, since no processor-induced latencies will be encountered by bus requests from
      * the device. In WAIT, as in all instructions, the PC points to the next instruction following the WAIT operation. Thus when
      * the service routine executes an RTI instruction, at the end of the routine, the program will resume at the instruction
      * following the WAIT. Note also that Floating Point, Power Fail, and Parity Traps will cause the processor to fall through the
      * WAIT loop.
      * @group Program Control Instructions
      */
    case WAIT extends Mnemonic

    /** Sends INIT on the UNIBUS for 10 ms. All devices on the UNIBUS are reset to their state at power up. In User Mode, the RESET
      * instruction is treated as a no-op. Within the PDP-11/60 processor, the Stack Limit and Memory Management Register, MMR0, are
      * initialized.
      * @group Program Control Instructions
      */
    case RESET extends Mnemonic

    /** MTPS is not implemented in the PDP-11/60. A processor trap through vector address 10 occurs for reserved instruction.
      * @group Program Control Instructions
      */
    case MTPS extends Mnemonic(OperandRole.Source)

    /** MFPS is not implemented in the PDP-11/60. A processor trap through vector address 10 occurs for reserved instruction.
      * @group Program Control Instructions
      */
    case MFPS extends Mnemonic(OperandRole.Destination)

    /** Set and clear condition code bits. Selectable combinations of these bits may be cleared or set together. Condition code bits
      * corresponding to bits in the condition code operator (Bits 0-3) are modified according to the sense of bit 4, the set/clear
      * bit of the operator. i.e. set the bit specified by bit 0, 1, 2 or 3, if bit 4 is a 1. Clear corresponding bits if bit 4 = 0.
      * Combinations of the above set or clear operation codes may be ORed together to form combined instructions.
      * @group Program Control Instructions
      */
    case NOP extends Mnemonic

    /** @see
      *   [[NOP]]
      * @group Program Control Instructions
      */
    case CLN extends Mnemonic

    /** @see
      *   [[NOP]]
      * @group Program Control Instructions
      */
    case SEN extends Mnemonic

    /** @see
      *   [[NOP]]
      * @group Program Control Instructions
      */
    case CLZ extends Mnemonic

    /** @see
      *   [[NOP]]
      * @group Program Control Instructions
      */
    case SEZ extends Mnemonic

    /** @see
      *   [[NOP]]
      * @group Program Control Instructions
      */
    case CLV extends Mnemonic

    /** @see
      *   [[NOP]]
      * @group Program Control Instructions
      */
    case SEV extends Mnemonic

    /** @see
      *   [[NOP]]
      * @group Program Control Instructions
      */
    case CLC extends Mnemonic

    /** @see
      *   [[NOP]]
      * @group Program Control Instructions
      */
    case SEC extends Mnemonic

    /** @see
      *   [[NOP]]
      * @group Program Control Instructions
      */
    case CCC extends Mnemonic

    /** @see
      *   [[NOP]]
      * @group Program Control Instructions
      */
    case SCC extends Mnemonic

    /** The contents of the destination register and source taken as 16-bit two's complement integers are multiplied and stored as a
      * 32-bit result in the destination register and the succeeding register if R is even. If an even register is specified the
      * 32-bit product is stored with the high 16 bits in the even register and the low 16 bits in the odd register. If R is odd,
      * only the low order 16 bits of the product is stored in the odd register. The condition codes are set according to the
      * internally generated 32-bit product. Note that the actual destination is R,Rv1 which reduces to just R when R is odd,
      * leaving the even numbered preceding register untouched.
      * @group Extended Instruction Set
      */
    case MUL extends Mnemonic(OperandRole.Register, OperandRole.Source)

    /** The 32-bit two's complement integer in R and Rv1 is divided by the source operand. The quotient is left in R; the remainder
      * in Rv1. Division will be performed so that the remainder is of the same sign as the dividend. R must be even.
      * @group Extended Instruction Set
      */
    case DIV extends Mnemonic(OperandRole.Register, OperandRole.Source)

    /** The contents of the register are shifted right or left the number of times specified by the source operand. The shift count
      * is taken as the low order 6 bits of the source operand. This number ranges from -32 to +31. Negative is a right shift and
      * positive is a left shift. For example, if the LSB 6 bits of the source are: 011111, then shift left 31 places; if 000001,
      * then shift left 1 place; if 111111, then shift right 1 place; if 100000, then shift right 32 places.
      * @group Extended Instruction Set
      */
    case ASH extends Mnemonic(OperandRole.Register, OperandRole.Source)

    /** The contents of the register pair R,Rv1 (R an even register, Rv1 an odd register) are treated as one 32 bit word, Rv1
      * containing bits 0 to 15, R bits 16 to 31, and are shifted right or left the number of times specified by the source operand.
      * The source contains a shift count taken as the low order 6 bits of the operand. This number ranges from -32 to +31. Negative
      * is a right shift and positive is a left shift. When the register chosen is an odd number, R and Rv1 are the same. In this
      * case the right shift becomes a rotate. The 16 bit word is rotated right the number of bits specified by the shift count
      * provided the count is less than or equal to 16. If an even register is specified the sign of the 32-bit operand is bit 15 of
      * the shifted result of the even register. If an odd register is specified, the sign is bit 15 of the shifted result in the
      * odd register.
      * @group Extended Instruction Set
      */
    case ASHC extends Mnemonic(OperandRole.Register, OperandRole.Source)

    /** The "Exclusive OR" of the register and destination operand is stored in the destination. Contents of register are
      * unaffected.
      * @group Extended Instruction Set
      */
    case XOR extends Mnemonic(OperandRole.Register, OperandRole.Destination)

    /** Adds the A argument to the B argument and stores the result in the A argument position on the stack. General register R is
      * used as the stack pointer for the operation.
      * @group Floating Instruction Set
      */
    case FADD extends Mnemonic(OperandRole.Register)

    /** Subtracts the B argument from the A argument and stores the result in the A argument position on the stack. General register
      * R is used as the stack pointer for the operation.
      * @group Floating Instruction Set...
      */
    case FSUB extends Mnemonic(OperandRole.Register)

    /** Multiplies the A argument by the B argument and stores the result in the A argument position on the stack. General register
      * R is used as the stack pointer for the operation.
      * @group Floating Instruction Set...
      */
    case FMUL extends Mnemonic(OperandRole.Register)

    /** Divides the A argument by the B argument and stores the result in the A argument position on the stack. If the divisor (B
      * argument) is equal to zero, the stack is left untouched. General register R is used as the stack pointer for the operation.
      * @group Floating Instruction Set...
      */
    case FDIV extends Mnemonic(OperandRole.Register)

    /** Determine if the instuction represented by the mnemonic requres all the roles provided.
      * @param roles
      *   The roles to be checked.
      * @return
      *   True, if all the roles provided match all the required roles of the [[Mnemonic]], false otherwise.
      */
    def requiring(roles: OperandRole*): Boolean = roles.forall(operandRoles.contains(_)) && roles.length == operandRoles.length
