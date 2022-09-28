package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** The [[PSECT]] directive may be used without a name or arguments. The [[PSECT]] directive allows a user to create program
  * sections and to share code and data among the sections he has created. In declaring the program sections (also called
  * p-sections), you may declare the attributes of the p-sections. This allows you to control memory allocation and at the same time
  * increases program modularity.
  *
  * @param name
  *   Establishes the program section name (default blank), which is specified as one to six Radix-50 characters. If this argument
  *   is omitted, a comma must appear in place of the name parameter.
  * @param arguments
  * @todo
  *   Enum args
  * @param comment
  *   The comment at the end of the directive.
  * @see
  *   [[ASECT]], [[CSECT]]
  */
final case class PSECT(name: String, arguments: Seq[String], comment: Comment) extends Directive
