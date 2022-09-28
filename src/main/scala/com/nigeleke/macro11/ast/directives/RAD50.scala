package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** The [[RAD50]] directive allows the user to generate data in Radix-50 packed format. Radix-50 form allows three characters to be
  * packed into sixteen bits (one word); therefore, any 6-character symbol can be stored in two consecutive words.
  *
  * @param param
  *   A delimited string respresenting a series of characters to be packed. The string must consist of the characters A through Z, 0
  *   through 9, dollar sign ($), period (.) and space ( ). An illegal printing character causes an error flag (Q) to be printed in
  *   the assembly listing. If fewer than three characters are to be packed, the string is packed left-justified within the word,
  *   and trailing spaces are assumed.
  * @param comment
  *   The comment at the end of the directive.
  */
final case class RAD50(param: DelimitedString, comment: Comment) extends Directive
