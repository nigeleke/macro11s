package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** The [[TITLE]] directive assigns a name to the object module. The name assigned is the first six non-blank, Radix-50 characters
  * following the [[TITLE]] directive. All spaces and/or tabs up to the first non-space/non-tab character following the [[TITLE]]
  * directive are ignored by MACRO-11 when evaluating the text string. Any characters beyond the first six are checked for ASCII
  * legality, but they are not used as part of the object module name.
  *
  * @param title
  *   An identifier of 1 or more Radix-50 characters.
  * @see
  *   [[SBTTL]]
  */
final case class TITLE(title: String) extends Directive
