package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** The [[IDENT]] directive allows the user to label the object module with the program version number, ```.IDENT /V1.00/```
  *
  * @param content
  *   A delimited string containing the identity.
  * @param comment
  *   The comment at the end of the directive.
  */
final case class IDENT(content: DelimitedString, comment: Comment) extends Directive
