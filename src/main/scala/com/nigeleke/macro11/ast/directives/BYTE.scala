package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** The [[BYTE]] directive is used to generate successive bytes of binary data in the object module.
  *
  * @param expressions
  *   Expressions that must be reduced to 8 bits of data or less. Each expression will be read as a 16-bit word expression, the
  *   high-order byte to be truncated. The high-order byte must be either all zeros or a truncation (T) error results. Multiple
  *   expressions must be separated by commas.
  * @param comment
  *   The comment at the end of the directive.
  * @see
  *   [[WORD]]
  */
final case class BYTE(expressions: List[Expression], comment: Comment) extends Directive
