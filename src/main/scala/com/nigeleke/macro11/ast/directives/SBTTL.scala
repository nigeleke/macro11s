package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** The [[SBTTL]] directive is used to produce a table of contents immediately preceding the assembly listing and to print the text
  * following the [[SBTTL]] directive on the second line of the header of each page in the listing. The subheading in the text will
  * be listed until altered by a subsequent [[SBTTL]] directive in the program.
  *
  * @param subtitle
  *   An identifier of 1 or more printable ASCII characters .
  * @see
  *   [[TITLE]]
  */
final case class SBTTL(subtitle: String) extends Directive
