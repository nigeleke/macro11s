package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** The [[BLKB]] directive reserves byte blocks in the object module.
  *
  * @param expression
  *   The number of bytes to reserve.
  * @param comment
  *   The comment at the end of the directive.
  * @see
  *   [[BLKW]]
  */
final case class BLKB(expression: Expression, comment: Comment) extends Directive
