package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** Immediate Conditional assembly directive.
  *
  * @see
  *   [[IIFBlank]], [[IIFCompare]], [[IIFDefined]], [[IIFIdentical]]
  */
trait IIF extends Directive

final case class IIFBlank(
    condition: String,
    argument: ExpressionTerm,
    instruction: Instruction,
    comment: Comment
) extends IIF

final case class IIFCompare(
    condition: String,
    expression: Expression,
    instruction: Instruction,
    comment: Comment
) extends IIF

final case class IIFDefined(
    condition: String,
    symbol: String,
    instruction: Instruction,
    comment: Comment
) extends IIF

final case class IIFIdentical(
    condition: String,
    argument1: ExpressionTerm,
    argument2: ExpressionTerm,
    instruction: Instruction,
    comment: Comment
) extends IIF
