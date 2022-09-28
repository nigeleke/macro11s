package com.nigeleke.macro11.parser

import com.nigeleke.macro11.ast.*
import com.nigeleke.macro11.ast.directives.*
import com.nigeleke.macro11.ast.Instruction.Mnemonic
import com.nigeleke.macro11.ast.Instruction.OperandRole
import com.nigeleke.macro11.parser.*

import scala.util.parsing.combinator.*

trait DirectiveParser extends InstructionParser with UtilityParser:

  private val delimitedString      = """(.)((?!\1).)*(\1)""".r ^^ { DelimitedString.from }
  private val delimitedRad50String = """(.)((?!\1)[A-Z0-9$. ])*(\1)""".r ^^ { DelimitedString.from }
  private val double               = """\d*(\.\d*)?""".r ^^ { BigDecimal(_) }
  private val digits               = """\d+""".r ^^ { identity }
  private val rad50Symbol          = """[A-Z0-9$.]*""".r ^^ { identity }

  private def asciiDirective = exact(".ASCII") ~> delimitedString ~ comment ^^ { case s ~ c => ASCII(s, c) }
  private def ascizDirective = exact(".ASCIZ") ~> delimitedString ~ comment ^^ { case s ~ c => ASCIZ(s, c) }
  private def asectDirective = exact(".ASECT") ~> comment ^^ { ASECT.apply }
  private def blkbDirective  = exact(".BLKB") ~> expression ~ comment ^^ { case e ~ c => BLKB(e, c) }
  private def blkwDirective  = exact(".BLKW") ~> expression ~ comment ^^ { case e ~ c => BLKW(e, c) }
  private def byteDirective = exact(".BYTE") ~> rep1sep(expression, separator) ~ comment ^^ { case es ~ c =>
    BYTE(es, c)
  }
  private def crossDirective = exact(".CROSS") ~> repsep(symbol, separator) ~ comment ^^ { case ss ~ c =>
    CROSS(ss, c)
  }
  private def csectDirective = exact(".CSECT") ~> rad50Symbol ~ comment ^^ { case n ~ c => CSECT(n, c) }
  private def enablDsablArgument =
    val first :: rest = EnablDsablArgument.values.toList.map(_.toString ^^ identity): @unchecked
    rest.foldLeft(first)((es, e) => es ||| e) ^^ { EnablDsablArgument.valueOf }
  private def dsablDirective = exact(".DSABL") ~> enablDsablArgument ~ comment ^^ { case a ~ c => DSABL(a, c) }
  private def enablDirective = exact(".ENABL") ~> enablDsablArgument ~ comment ^^ { case a ~ c => ENABL(a, c) }
  private def endDirective   = exact(".END") ~> opt(expression) ~ comment ^^ { case e ~ c => END(e, c) }
  private def endcDirective  = exact(".ENDC") ~> comment ^^ { ENDC.apply }
  private def evenDirective  = exact(".EVEN") ~> comment ^^ { EVEN.apply }
  private def flt2Directive  = exact(".FLT2") ~> repsep(double, separator) ~ comment ^^ { case fs ~ c => FLT2(fs, c) }
  private def flt4Directive  = exact(".FLT4") ~> repsep(double, separator) ~ comment ^^ { case fs ~ c => FLT4(fs, c) }
  private def globalDirective = exact(".GLOBL") ~> rep1sep(symbol, separator) ~ comment ^^ { case ss ~ c =>
    GLOBL(ss, c)
  }
  private def identDirective = exact(".IDENT") ~> delimitedString ~ comment ^^ { case s ~ c => IDENT(s, c) }
  private def ifDirectiveBlank =
    ("B" | "NB") ~
      (separator ~> macroArgument)
      ~ comment ^^ { case cond ~ m ~ c => IFBlank(cond, m, c) }
  private def ifDirectiveCompare =
    ("EQ" | "NE" | "LT" | "LE" | "GE" | "GT") ~
      (separator ~> expression)
      ~ comment ^^ { case cond ~ e ~ c => IFCompare(cond, e, c) }
  private def ifDirectiveDefined =
    ("DF" | "NDF") ~
      (separator ~> symbol) ~
      comment ^^ { case cond ~ s ~ c => IFDefined(cond, s, c) }
  private def ifDirectiveIdentical =
    ("IDN" | "DIF") ~
      (separator ~> macroArgument) ~
      (separator ~> macroArgument) ~
      comment ^^ { case cond ~ m1 ~ m2 ~ c => IFIdentical(cond, m1, m2, c) }
  private def ifDirective   = exact(".IF") ~> (ifDirectiveBlank | ifDirectiveCompare | ifDirectiveDefined | ifDirectiveIdentical)
  private def iffDirective  = exact(".IFF") ~> comment ^^ { IFF.apply }
  private def iftDirective  = exact(".IFT") ~> comment ^^ { IFT.apply }
  private def iftfDirective = exact(".IFTF") ~> comment ^^ { IFTF.apply }
  private def iifDirectiveBlank =
    ("B" | "NB") ~
      (separator ~> macroArgument) ~
      (separator ~> instruction) ~
      comment ^^ { case cond ~ m ~ i ~ c => IIFBlank(cond, m, i, c) }
  private def iifDirectiveCompare =
    ("EQ" | "NE" | "LT" | "LE" | "GE" | "GT") ~
      (separator ~> expression) ~
      (separator ~> instruction) ~
      comment ^^ { case cond ~ e ~ i ~ c => IIFCompare(cond, e, i, c) }
  private def iifDirectiveDefined =
    ("DF" | "NDF") ~
      (separator ~> symbol) ~
      (separator ~> instruction) ~
      comment ^^ { case cond ~ s ~ i ~ c => IIFDefined(cond, s, i, c) }
  private def iifDirectiveIdentical =
    ("IDN" | "DIF") ~
      (separator ~> macroArgument) ~
      (separator ~> macroArgument) ~
      (separator ~> instruction)
      ~ comment ^^ { case cond ~ m1 ~ m2 ~ i ~ c => IIFIdentical(cond, m1, m2, i, c) }
  private def iifDirective =
    exact(".IIF") ~> (iifDirectiveBlank | iifDirectiveCompare | iifDirectiveDefined | iifDirectiveIdentical)
  private def includeDirective = exact(".INCLUDE") ~> delimitedString ~ comment ^^ { case s ~ c => INCLUDE(s, c) }
  private def libraryDirective = exact(".LIBRARY") ~> delimitedString ~ comment ^^ { case s ~ c => LIBRARY(s, c) }
  private def limitDirective   = exact(".LIMIT") ~> comment ^^ { LIMIT.apply }
  private def listDirective    = exact(".LIST") ~> opt(symbol) ~ comment ^^ { case s ~ c => LIST(s, c) }
  private def nlistDirective   = exact(".NLIST") ~> opt(symbol) ~ comment ^^ { case s ~ c => NLIST(s, c) }
  private def noCrossDirective = exact(".NOCROSS") ~> repsep(symbol, separator) ~ comment ^^ { case ss ~ c =>
    NOCROSS(ss, c)
  }
  private def oddDirective = exact(".ODD") ~> comment ^^ { ODD.apply }
  private def packedDirective = exact(".PACKED") ~> digits ~ opt(separator ~> symbol) ~ comment ^^ { case d ~ s ~ c =>
    PACKED(d, s, c)
  }
  private def pageDirective = exact(".PAGE") ~> comment ^^ { PAGE.apply }
  private def psectArg = ("RO" | "RW" | "I" | "D" | "GBL" | "LCL" | "ABS" | "REL" | "CON" | "OVR" | "SAV" | "NOSAV") ^^ { identity }
  private def psectDirective = exact(".PSECT") ~> rad50Symbol ~ (separator ~> rep1sep(psectArg, separator)) ~ comment ^^ {
    case n ~ pas ~ c => PSECT(n, pas, c)
  }
  private def rad50Directive = exact(".RAD50") ~> delimitedRad50String ~ comment ^^ { case s ~ c => RAD50(s, c) }
  private def radixDirective = exact(".RADIX") ~> opt("2" | "8" | "10") ~ comment ^^ { case r ~ c =>
    RADIX(r.getOrElse(""), c)
  }
  private def remDirective     = exact(".REM") ~> anyCharacter ^^ { REM.apply }
  private def restoreDirective = exact(".RESTORE") ~> comment ^^ { RESTORE.apply }
  private def saveDirective    = exact(".SAVE") ~> comment ^^ { SAVE.apply }
  private def sbttlDirective   = exact(".SBTTL") ~> anyString ^^ { SBTTL.apply }
  private def titleDirective   = exact(".TITLE") ~> anyString ^^ { TITLE.apply }
  private def weakDirective    = exact(".WEAK") ~> rep1sep(symbol, separator) ~ comment ^^ { case ss ~ c => WEAK(ss, c) }
  private def wordDirective = exact(".WORD") ~> rep1sep(expression, separator) ~ comment ^^ { case es ~ c =>
    WORD(es, c)
  }

  def directive: Parser[Directive] =
    asciiDirective ||| ascizDirective ||| asectDirective |||
      blkbDirective ||| blkwDirective ||| byteDirective |||
      crossDirective ||| csectDirective |||
      dsablDirective |||
      enablDirective ||| endDirective ||| endcDirective ||| evenDirective |||
      flt2Directive ||| flt4Directive |||
      globalDirective |||
      identDirective ||| ifDirective ||| iffDirective ||| iftDirective ||| iftfDirective ||| iifDirective ||| includeDirective |||
      libraryDirective ||| limitDirective ||| listDirective |||
      nlistDirective ||| noCrossDirective |||
      oddDirective |||
      packedDirective ||| pageDirective ||| psectDirective |||
      rad50Directive ||| radixDirective ||| remDirective ||| restoreDirective |||
      saveDirective ||| sbttlDirective |||
      titleDirective |||
      weakDirective ||| wordDirective
