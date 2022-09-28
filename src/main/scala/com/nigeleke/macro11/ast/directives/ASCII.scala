package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** The [[ASCII]] directive translates character strings into their 7-bit ASCII equivalents and stores them in the object module. A
  * non-printing character can be expressed only by enclosing its equivalent octal value within angle brackets. Each set of angle
  * brackets so used represents a single character.
  *
  * @todo
  *   Allow more than DelimitedString
  * @param param
  * @param comment
  *   The comment at the end of the directive.
  */
final case class ASCII(param: DelimitedString, comment: Comment) extends Directive
