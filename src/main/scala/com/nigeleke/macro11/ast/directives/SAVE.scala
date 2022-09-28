package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** [[SAVE]] stores the current program section context on the top program section context stack, while leaving the current section
  * context in effect. If the stack is full when [[SAVE]] is issued, an error (A) occurs. The stack can handle 16 [[SAVE]]s. The
  * section context includes the values of the current location and the maximum value assigned to the location counter in the
  * program section.
  *
  * @param comment
  *   The comment at the end of the directive.
  * @see
  *   [[RESTORE]]
  */
final case class SAVE(comment: Comment) extends Directive
