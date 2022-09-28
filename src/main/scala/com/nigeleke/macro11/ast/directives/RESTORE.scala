package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** The [[RESTORE]] directive retrieves the program section from the top of the program section context stack. If the stack is empty
  * when [[RESTORE]] is issued, an error (A) occurs. When [[RESTORE]] retrieves a program section, it restores the current location
  * counter to the value it had when the program section was saved.
  *
  * @param comment
  *   The comment at the end of the directive.
  * @see
  *   [[SAVE]]
  */
final case class RESTORE(comment: Comment) extends Directive
