package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** The [[DSABL]] directive disables assembler functions as determined by the argument.
  *
  * @param argument
  *   One or more of the optional symbolic arguments.
  * @todo
  *   Allow more than one argument.
  * @param comment
  *   The comment at the end of the directive.
  * @see
  *   [[ENABL]]
  */
final case class DSABL(argument: EnablDsablArgument, comment: Comment) extends Directive
