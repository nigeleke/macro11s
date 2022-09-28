package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** The [[PACKED]] directive generates packed decimal data, 2 digits per byte. Arithmetic and operational properties of packed
  * decimals are similar to those of numeric strings.
  *
  * @param decimal
  *   A decimal number from 0 to 31 ,,10,, digits long. Each digit must be in the range 0 to 9. The number may have a sign, but it
  *   is not required and is not counted as a digit.
  * @param maybeSymbol
  *   If provided, the symbol is assigned a value equivalent to the number of decimal digits in the string.
  * @param comment
  *   The comment at the end of the directive.
  */
final case class PACKED(decimal: String, maybeSymbol: Option[String], comment: Comment) extends Directive
