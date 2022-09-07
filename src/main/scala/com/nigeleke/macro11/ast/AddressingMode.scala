package com.nigeleke.macro11.ast

sealed trait AddressingMode
case class RegisterMode(r: String)                  extends AddressingMode
case class RegisterDeferredMode(re: String)         extends AddressingMode
case class AutoIncrementMode(re: String)            extends AddressingMode
case class AutoIncrementDeferredMode(re: String)    extends AddressingMode
case class AutoDecrementMode(re: String)            extends AddressingMode
case class AutoDecrementDeferredMode(re: String)    extends AddressingMode
case class IndexMode(e: String, re: String)         extends AddressingMode
case class IndexDeferredMode(e: String, re: String) extends AddressingMode
case class ImmediateMode(e: String)                 extends AddressingMode
case class AbsoluteMode(e: String)                  extends AddressingMode
case class RelativeMode(e: String)                  extends AddressingMode
case class RelativeDeferredMode(e: String)          extends AddressingMode
case class Branch(a: String)                        extends AddressingMode
