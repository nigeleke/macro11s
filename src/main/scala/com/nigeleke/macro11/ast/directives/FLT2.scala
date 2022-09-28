package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** [[FLT2]] causes two words of storage to be generated for each argument. As in the WORD directive, the arguments are evaluated
  * and the results are stored in the object module .
  *
  * @param floats
  *   A separated list of numbers to be stored.
  * @param comment
  *   The comment at the end of the directive.
  * @see
  *   [[FLT4]]
  */
final case class FLT2(floats: List[BigDecimal], comment: Comment) extends Directive
