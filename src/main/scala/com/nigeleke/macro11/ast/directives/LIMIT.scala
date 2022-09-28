package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** When the [[LIMIT]] directive is specified in the program, MACRO-11 generates the following instruction: ```.BLKW 2``` Later, at
  * link time, the lowest address in the load image (the initial value of SP) is inserted into the first reserved word and the
  * address of the first free word following the image is inserted into the second reserved word.
  *
  * @param comment
  *   The comment at the end of the directive.
  */
final case class LIMIT(comment: Comment) extends Directive
