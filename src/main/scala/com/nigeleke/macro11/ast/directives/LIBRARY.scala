package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** The [[LIBRARY]] directive adds a file name to a macro library list that is searched. A library list is searched whenever a MCALL
  * or an undefined opcode is encountered within a MACRO-11 program. The libraries that makde up the list are searched in the
  * reverse order in which they were specified in the assembler.
  *
  * @param file
  *   Delimited string providing relative (from current directory) or absolute path to the file to be searched.
  * @param comment
  *   The comment at the end of the directive.
  */
final case class LIBRARY(file: DelimitedString, comment: Comment) extends Directive
