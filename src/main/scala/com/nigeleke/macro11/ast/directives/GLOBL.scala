package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** The [[GLOBL]] directive is provided to define (and thus provide linkage to) symbols not otherwise defined as global symbols
  * within a module.
  *
  * @param symbols
  *   A separated list of symbols to be declared global.
  * @param comment
  *   The comment at the end of the directive.
  */
final case class GLOBL(symbols: List[String], comment: Comment) extends Directive
