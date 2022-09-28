package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** IAS and RSX-11M assembly-language programs use the [[PSECT]] and [[ASECT]] directives exclusively, because the [[PSECT]]
  * directive provides all the capabilities of the [[ASECT]] directive defined for other PDP-11 assemblers. MACRO-11 will accept
  * both [[ASECT]] and [[CSECT]] directives, but assembles them as though they were [[PSECT]] directives with default values listed
  * in [TODO:]
  *
  * @param comment
  *   The comment at the end of the directive.
  * @see
  *   [[PSECT]]
  */
final case class ASECT(comment: Comment) extends Directive
