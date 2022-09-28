package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** The [[WEAK]] directive is used to specify symbols that are either defined externally in another module or defined globally in
  * the current module. This directive suppresses object library searches for specified external symbols.
  *
  * @param symbols
  *   A separated list of symbols to be defined as weak.
  * @param comment
  *   The comment at the end of the directive.
  */
final case class WEAK(symbols: List[String], comment: Comment) extends Directive
