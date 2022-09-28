package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** Numbers used in a MACRO-11 source program are initially considered to be octal values; however, with the [[RADIX]] directive you
  * can declare alternate radices applicable throughout the source program or within specific portions of the program.
  *
  * @param param
  *   Represents one of the three radices: 2, 8 and 10. Any value other than null or one of the three acceptable radices will cause
  *   an error in the assembly listing. If the argument n is not specified the octal default the octal default is assumed. The
  *   argument (n) is always read as a decimal value .
  * @param comment
  *   The comment at the end of the directive.
  */
final case class RADIX(param: String, comment: Comment) extends Directive
