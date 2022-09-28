package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** When MACRO-11 encounters a valid occurrence of the [[END]] directive, it terminates the current assembly pass. Any text beyond
  * this point in the current source file, or in additional source files identified the command line, will be ignored.
  *
  * @param expression
  *   An optional expression value which, if present, indicates the program-entry point, which the transfer address where the
  *   program begins.
  * @param comment
  *   The comment at the end of the directive.
  */
final case class END(expression: Option[Expression], comment: Comment) extends Directive
