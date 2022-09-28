package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** The [[WORD]] directive is used to generate successive words of data in the object module.
  *
  * @param expressions
  *   Expressions that must reduce to 16 bits of data or less. Multiple expressions must be separated by commas.
  * @param comment
  *   The comment at the end of the directive.
  * @see
  *   [[BYTE]]
  */
final case class WORD(expressions: List[Expression], comment: Comment) extends Directive
