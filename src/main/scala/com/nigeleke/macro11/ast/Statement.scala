package com.nigeleke.macro11.ast

case class Statement(labels: List[Label], maybeInstruction: Option[Instruction], comment: Comment)
