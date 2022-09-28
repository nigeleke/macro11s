package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** The [[ASCIZ]] directive is similar to the [[ASCII]] directive, except that a zero byte is automatically inserted as the final
  * character of the string. Thus, when a list or text string has been created with an [[ASCIZ]] directive, a search for the null
  * character in the last byte can effectively determine the end of the string.
  *
  * @todo
  *   Allow more than DelimitedString
  * @param param
  * @param comment
  *   The comment at the end of the directive.
  */
final case class ASCIZ(param: DelimitedString, comment: Comment) extends Directive
