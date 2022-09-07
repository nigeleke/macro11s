package com.nigeleke.macro11.ast

sealed trait StatementLine
case class InstructionLine(maybeLabels: List[Label], maybeInstruction: Option[Instruction], maybeComment: Option[Comment])
    extends StatementLine
case class DirectiveLine(directive: Directive) extends StatementLine
