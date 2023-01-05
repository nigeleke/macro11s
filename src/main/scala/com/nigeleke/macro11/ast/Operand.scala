package com.nigeleke.macro11.ast

/** When an operator is an [[Instruction]] mnemonic (op code), the [[Operand]] field contains program variables that are to be
  * evaluated/manipulated by the operator. The [[Operand]] field may also supply arguments to MACRO-11 [[Directive]]s and macro
  * calls.
  *
  * [[Operand]]s may be [[Expression]]s or symbols, depending on the operator.
  *
  * Multiple [[Expression]]s used in the operand field of a MACRO-11 statement must be separated by a comma; multiple symbols
  * similarly used may be delimited by any legal separator (a comma, tab, and/or space). An [[Operand]] should be preceded by an
  * operator field; if it is not, the statement is treated by MACRO-11 as an implicit WORD directive.
  */
trait Operand

object Operand:

  /** [[Operand]] used by the majority of single and double operand [[Instruction]]s.
    * @param mode
    *   One of the regular application of the operand.
    */
  final case class AddressingModeOperand(mode: AddressingMode) extends Operand

  /** [[Operand]] used by Branch and Jump [[Instruction]]s.
    * @param address
    *   An expression, providing the target address for the branch if conditions are met.
    */
  final case class AddressOffsetOperand(address: Expression) extends Operand

  /** [[Operand]] used by some [[Instruction]]s such as [[JSR]], [[MUL]], [[FMUL]], which reference a specific register.
    * @param register
    *   An expression referencing one of the registers.
    */
  final case class RegisterOperand(register: Expression) extends Operand

  /** [[Operand]] used by the [[MARK]] [[Instruction]].
    * @param count
    *   The number of words to be removed from the stack when [[MARK]] is executed.
    */
  final case class ParameterCountOperand(count: Expression) extends Operand

  /** [[Operand]] used by the [[TRAP]] [[Instruction]].
    * @param id
    *   An expression from 0 to 377,,8,, identifying the [[TRAP]].
    */
  final case class TrapParameterOperand(id: Expression) extends Operand
