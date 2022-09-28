package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** The [[REM]] directive allows a programmer to insert a block of comments into a MACRO-11 source program without having to precede
  * the lines with the comment character (;). The text between the delimiting characters is treated as any number of lines.
  *
  * @param commentChar
  *   A character that marks the end of the comment block when the character reoccurs.
  * @unsupported
  */
final case class REM(commentChar: String) extends Directive
