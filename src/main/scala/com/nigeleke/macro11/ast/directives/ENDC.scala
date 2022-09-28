package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** End of conditional assembly block.
  *
  * @param comment
  *   The comment at the end of the directive.
  * @see
  *   [[IF]]
  */
final case class ENDC(comment: Comment) extends Directive
