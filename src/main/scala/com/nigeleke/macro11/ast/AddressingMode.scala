package com.nigeleke.macro11.ast

/** In the PDP-11 family, all operand addressing is accomplished through the general purpose registers. To specify the location of
  * data (an operand address), one of eight registers is selected with an accompanying addressing mode. Each instruction specifies:
  *
  *   1. Function to be performed (operation code)
  *   1. General purpose register to be used when locating the source operand and/or destination operand (where required)
  *   1. Addressing mode, which specifies how the selected registers are to be used
  *
  * The instruction format and addressing techniques available to the programmer are of particular importance. This combination of
  * addressing modes and the instruction set provides the PDP-11 family with a unique number of capabilities. The PDP-11 is designed
  * to handle structured data efficiently and with flexibility. The general purpose registers implement these functions in the
  * following ways, by acting:
  *
  *   1. As accumulators: holding the data to be manipulated
  *   1. As pointers: the contents of the register are the address of the operand, rather than the operand itself
  *   1. As index registers: the contents of the register are added to an additional word of the instruction to produce the address
  *      of the operand; this capability allows easy access to variable entries in a list.
  */
trait AddressingMode

object AddressingMode:
  /** Register contains operand.
    * @param re
    *   A register expression.
    */
  final case class Register(re: Expression) extends AddressingMode

  /** Register contains the address of the operand.
    * @param re
    *   A register expression.
    */
  final case class RegisterDeferred(r: Expression) extends AddressingMode

  /** Register is used as a pointer to sequential data, then incremented by one for byte and two for word instruction. R6-R7 are
    * always incremented by two.
    * @param re
    *   A register expression.
    */
  final case class AutoIncrement(re: Expression) extends AddressingMode

  /** Register is first used as a pointer to a word containing the address of the operand, then incremented (always by two, even for
    * byte instructions).
    * @param re
    *   A register expression.
    */
  final case class AutoIncrementDeferred(re: Expression) extends AddressingMode

  /** Register is decremented and then used as a pointer to sequential data. R0-R5 are decremented by one for byte and by two for
    * word instructions. R6-R7 are always decremented by two.
    * @param re
    *   A register expression.
    */
  final case class AutoDecrement(re: Expression) extends AddressingMode

  /** Register is decremented (always by two, even for byte instructions) and the used as a pointer to a word containing the address
    * of the operand.
    * @param re
    *   A register expression.
    */
  final case class AutoDecrementDeferred(re: Expression) extends AddressingMode

  /** Value X is added to Rn to produce address of operand. Neither X nor Rn is modified. X, the index calue is always found in the
    * next memory location and increments the PC.
    * @param xe
    *   An index expression.
    * @param re
    *   A register expression.
    */
  final case class Index(xe: Expression, re: Expression) extends AddressingMode

  /** Value X (the index is always found in the next memory location and increments the PC by two) and Rn are added and the sum is
    * used as a pointer to a word containing the address of the operand. Neither X nor Rn is modified.
    * @param xe
    *   An index expression.
    * @param re
    *   A register expression.
    */
  final case class IndexDeferred(xe: Expression, re: Expression) extends AddressingMode

  /** Operand is contained in the instruction.
    * @param e
    *   An immediae value expression.
    */
  final case class Immediate(e: Expression) extends AddressingMode

  /** Absolute address in contained in the instruction.
    * @param e
    *   An absolute address expression.
    */
  final case class Absolute(e: Expression) extends AddressingMode

  /** Address of A, relative to the instruction, is contained in the instruction.
    * @param e
    *   An address expression.
    */
  final case class Relative(e: Expression) extends AddressingMode

  /** Address of A, relative to the instruction, is contained in the instruction. Address of the oeprand is contained in A.
    * @param e
    *   An address expression.
    */
  final case class RelativeDeferred(e: Expression) extends AddressingMode
