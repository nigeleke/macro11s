package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** The [[CROSS]] and the [[NOCROSS]] directives control which symbols are included in the cross-reference listing produced by the
  * MACRO-11 assembler. These directives have an effect only if the /C[R] or the /CROSS qualifier was used in the command line to
  * select the cross-reference capability. By default, the cross-reference listing includes the definition and all the references to
  * every user symbol in the module. The cross-reference listing can be disabled for all symbols or for a specified list of symbols.
  *
  * @param symbols
  *   Legal symbolic names. When multiple symbols are specified, they are separated by any legal separator (comma, space, and/or
  *   tab).
  * @param comment
  *   The comment at the end of the directive.
  * @see
  *   [[NOCROSS]]
  */
final case class CROSS(symbols: List[String], comment: Comment) extends Directive
