package com.nigeleke.macro11.ast

trait Operand

object Operand:
  trait AddressingModeOperand                         extends Operand
  case class RegisterMode(r: String)                  extends AddressingModeOperand
  case class RegisterDeferredMode(re: String)         extends AddressingModeOperand
  case class AutoIncrementMode(re: String)            extends AddressingModeOperand
  case class AutoIncrementDeferredMode(re: String)    extends AddressingModeOperand
  case class AutoDecrementMode(re: String)            extends AddressingModeOperand
  case class AutoDecrementDeferredMode(re: String)    extends AddressingModeOperand
  case class IndexMode(e: String, re: String)         extends AddressingModeOperand
  case class IndexDeferredMode(e: String, re: String) extends AddressingModeOperand
  case class ImmediateMode(e: String)                 extends AddressingModeOperand
  case class AbsoluteMode(e: String)                  extends AddressingModeOperand
  case class RelativeMode(e: String)                  extends AddressingModeOperand
  case class RelativeDeferredMode(e: String)          extends AddressingModeOperand
  case class AddressOffsetOperand(a: String)          extends Operand
  case class RegisterOperand(r: String)               extends Operand
  case class ParameterCountOperand(pc: String)        extends Operand
  case class TrapParameterOperand(tp: String)         extends Operand
