package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** Conditional assembly directive.
  *
  * @see
  *   [[IFBlank]], [[IFCompare]], [[IFDefined]], [[IFIdentical]], [[ENDC]]
  */
trait IF extends Directive

final case class IFBlank(
    condition: String,
    argument: ExpressionTerm,
    comment: Comment
) extends IF

final case class IFCompare(condition: String, expression: Expression, comment: Comment) extends IF

final case class IFDefined(condition: String, symbol: String, comment: Comment) extends IF

final case class IFIdentical(condition: String, argument1: ExpressionTerm, argument2: ExpressionTerm, comment: Comment) extends IF
