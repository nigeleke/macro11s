package com.nigeleke.macro11.ast

/** A source [[Program]] is composed of assembly-language statements. Each statement must be completed on one line
  * ([[ProgramLine]]). Although a line may contain 132 characters (a longer line causes an error (L) in the assembly listing), a
  * line of 80 characters is recommended because of constraints imposed by listing format and terminal line size.
  *
  * @param lines
  *   Each parsed line of the program source code.
  */
final case class Program(lines: List[ProgramLine])
