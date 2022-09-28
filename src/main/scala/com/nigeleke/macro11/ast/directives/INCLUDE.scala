package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** The [[INCLUDE]] directive is used to insert a source file within the source file currently being used. When this directive is
  * encountered, the current source file is stacked and the source file specified by the directive is read into memory. When the end
  * of the specified source file is reached, the original source file is popped from the stack and assembly resumes at the line
  * following the directive. The maximum nesting level of source files specified by the [[INCLUDE]] directive is five.
  *
  * @param file
  *   Delimited string providing relative (from current directory) or absolute path to the file to be included.
  * @param comment
  *   The comment at the end of the directive.
  */
final case class INCLUDE(file: DelimitedString, comment: Comment) extends Directive
