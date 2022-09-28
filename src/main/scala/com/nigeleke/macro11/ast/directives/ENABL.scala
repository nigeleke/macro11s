package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** The [[ENABL]] directive enables assembler functions as determined by the argument.
  *
  * @param argument
  *   One or more of the optional symbolic arguments.
  * @todo
  *   Allow more than one argument.
  * @param comment
  *   The comment at the end of the directive.
  * @see
  *   [[DSABL]]
  */
final case class ENABL(argument: EnablDsablArgument, comment: Comment) extends Directive
