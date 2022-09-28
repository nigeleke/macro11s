package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** The [[ODD]] directive ensures that the current location an odd value by adding 1 if the current value is even. If the current
  * location counter is already odd, no action is taken.
  *
  * @param comment
  *   The comment at the end of the directive.
  * @see
  *   [[EVEN]]
  */
final case class ODD(comment: Comment) extends Directive
