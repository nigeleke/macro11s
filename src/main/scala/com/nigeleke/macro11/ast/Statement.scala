package com.nigeleke.macro11.ast

/** A MACRO-11 statement is:
  *
  * ````
  * [Label(s):] Operator Operand(s) [; Comment]
  * ````
  *
  * @param labels
  *   The list of labels (maybe zero length), preceeding the operator.
  * @param maybeInstruction
  *   The instruction, if present,
  * @param comment
  *   The trailing comment. Any empty string represents no comment.
  */
final case class Statement(labels: List[Label], maybeInstruction: Option[Instruction], comment: Comment) extends ProgramLine
