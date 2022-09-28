package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** If the condition tested upon entering the conditional assembly block is false, the code following this directive, and continuing
  * up to the next occurrence of a subconditional directive or to the end of the conditional assembly block, is to be included in
  * the program.
  *
  * @param comment
  *   The comment at the end of the directive.
  * @see
  *   [[IF]]
  */
final case class IFF(comment: Comment) extends Directive
