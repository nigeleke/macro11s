package com.nigeleke.macro11.ast

import com.nigeleke.macro11.ast.Comment
import com.nigeleke.macro11.ast.DelimitedString
import com.nigeleke.macro11.ast.Directive
import com.nigeleke.macro11.ast.Expression
import com.nigeleke.macro11.ast.ExpressionTerm
import com.nigeleke.macro11.ast.Instruction
import com.nigeleke.macro11.ast.ProgramLine
import com.nigeleke.macro11.ast.directives.EnablDsablArgument

/** A MACRO-11 directive is placed in the operator field of a source line. Only one directive is allowed per source line. Each
  * directive may have a blank operand field or one or more operands. Legal operands differ with each directive.
  *
  * General assembler directives are divided into the following categories:
  *
  *   1. Listing control
  *   1. Function control
  *   1. Data storage
  *   1. Radix and numeric control
  *   1. Location counter control
  *   1. Terminator
  *   1. Program sectioning and boundaries
  *   1. Symbol control
  *   1. Conditional assembly
  *   1. File control
  */
trait Directive extends ProgramLine
