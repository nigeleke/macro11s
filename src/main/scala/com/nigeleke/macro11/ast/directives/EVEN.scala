package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** The [[EVEN]] directive ensures that the current location an even value by adding 1 if the current value is odd. If the current
  * location counter is already even, no action is taken.
  *
  * @param comment
  *   The comment at the end of the directive.
  * @see
  *   [[ODD]]
  */
final case class EVEN(comment: Comment) extends Directive
