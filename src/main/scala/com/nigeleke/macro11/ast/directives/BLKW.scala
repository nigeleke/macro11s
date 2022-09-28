package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** The [[BLKW]] directive reserves word blocks in the object module.
  *
  * @param expression
  *   The number of words to reserve.
  * @param comment
  *   The comment at the end of the directive.
  * @see
  *   [[BLKB]]
  */
final case class BLKW(expression: Expression, comment: Comment) extends Directive
