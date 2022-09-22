package com.nigeleke.macro11.parser

import com.nigeleke.macro11.ast.*
import com.nigeleke.macro11.ast.Instruction.Mnemonic
import com.nigeleke.macro11.ast.Instruction.OperandRole
import com.nigeleke.macro11.parser.*

import scala.util.parsing.combinator.*

trait DirectiveParser extends InstructionParser with UtilityParser:

  private val delimitedString      = """(.)((?!\1).)*(\1)""".r ^^ { DelimitedString(_) }
  private val delimitedRad50String = """(.)((?!\1)[A-Z0-9$. ])*(\1)""".r ^^ { DelimitedString(_) }
  private val double               = """\d*(\.\d*)?""".r ^^ { BigDecimal(_) }
  private val digits               = """\d+""".r ^^ { identity }
  private val rad50Symbol          = """[A-Z0-9$.]*""".r ^^ { identity }

  private def asciiDirective = exact(".ASCII") ~> delimitedString ~ comment ^^ { case s ~ c => AsciiDirective(s, c) }
  private def ascizDirective = exact(".ASCIZ") ~> delimitedString ~ comment ^^ { case s ~ c => AscizDirective(s, c) }
  private def asectDirective = exact(".ASECT") ~> comment ^^ { ASectDirective.apply }
  private def blkbDirective  = exact(".BLKB") ~> expression ~ comment ^^ { case e ~ c => BlkbDirective(e, c) }
  private def blkwDirective  = exact(".BLKW") ~> expression ~ comment ^^ { case e ~ c => BlkwDirective(e, c) }
  private def byteDirective = exact(".BYTE") ~> rep1sep(expression, separator) ~ comment ^^ { case es ~ c =>
    ByteDirective(es, c)
  }
  private def crossDirective = exact(".CROSS") ~> repsep(symbol, separator) ~ comment ^^ { case ss ~ c =>
    CrossDirective(ss, c)
  }
  private def csectDirective = exact(".CSECT") ~> rad50Symbol ~ comment ^^ { case n ~ c => CSectDirective(n, c) }
  private def enablDsablArgument =
    val first :: rest = EnablDsablArgument.values.toList.map(_.toString ^^ identity): @unchecked
    rest.foldLeft(first)((es, e) => es ||| e) ^^ { EnablDsablArgument.valueOf }
  private def dsablDirective = exact(".DSABL") ~> enablDsablArgument ~ comment ^^ { case a ~ c => DsablDirective(a, c) }
  private def enablDirective = exact(".ENABL") ~> enablDsablArgument ~ comment ^^ { case a ~ c => EnablDirective(a, c) }
  private def endDirective   = exact(".END") ~> opt(expression) ~ comment ^^ { case e ~ c => EndDirective(e, c) }
  private def endcDirective  = exact(".ENDC") ~> comment ^^ { EndcDirective.apply }
  private def evenDirective  = exact(".EVEN") ~> comment ^^ { EvenDirective.apply }
  private def flt2Directive  = exact(".FLT2") ~> repsep(double, separator) ~ comment ^^ { case fs ~ c => Flt2Directive(fs, c) }
  private def flt4Directive  = exact(".FLT4") ~> repsep(double, separator) ~ comment ^^ { case fs ~ c => Flt4Directive(fs, c) }
  private def globalDirective = exact(".GLOBAL") ~> rep1sep(symbol, separator) ~ comment ^^ { case ss ~ c =>
    GlobalDirective(ss, c)
  }
  private def identDirective = exact(".IDENT") ~> delimitedString ~ comment ^^ { case s ~ c => IdentDirective(s, c) }
  private def ifDirectiveBlank =
    ("B" | "NB") ~
      (separator ~> macroArgument)
      ~ comment ^^ { case cond ~ m ~ c => IfDirectiveBlank(cond, m, c) }
  private def ifDirectiveCompare =
    ("EQ" | "NE" | "LT" | "LE" | "GE" | "GT") ~
      (separator ~> expression)
      ~ comment ^^ { case cond ~ e ~ c => IfDirectiveCompare(cond, e, c) }
  private def ifDirectiveDefined =
    ("DF" | "NDF") ~
      (separator ~> symbol) ~
      comment ^^ { case cond ~ s ~ c => IfDirectiveDefined(cond, s, c) }
  private def ifDirectiveIdentical =
    ("IDN" | "DIF") ~
      (separator ~> macroArgument) ~
      (separator ~> macroArgument) ~
      comment ^^ { case cond ~ m1 ~ m2 ~ c => IfDirectiveIdentical(cond, m1, m2, c) }
  private def ifDirective   = exact(".IF") ~> (ifDirectiveBlank | ifDirectiveCompare | ifDirectiveDefined | ifDirectiveIdentical)
  private def iffDirective  = exact(".IFF") ~> comment ^^ { IffDirective.apply }
  private def iftDirective  = exact(".IFT") ~> comment ^^ { IftDirective.apply }
  private def iftfDirective = exact(".IFTF") ~> comment ^^ { IftfDirective.apply }
  private def iifDirectiveBlank =
    ("B" | "NB") ~
      (separator ~> macroArgument) ~
      (separator ~> instruction) ~
      comment ^^ { case cond ~ m ~ i ~ c => IifDirectiveBlank(cond, m, i, c) }
  private def iifDirectiveCompare =
    ("EQ" | "NE" | "LT" | "LE" | "GE" | "GT") ~
      (separator ~> expression) ~
      (separator ~> instruction) ~
      comment ^^ { case cond ~ e ~ i ~ c => IifDirectiveCompare(cond, e, i, c) }
  private def iifDirectiveDefined =
    ("DF" | "NDF") ~
      (separator ~> symbol) ~
      (separator ~> instruction) ~
      comment ^^ { case cond ~ s ~ i ~ c => IifDirectiveDefined(cond, s, i, c) }
  private def iifDirectiveIdentical =
    ("IDN" | "DIF") ~
      (separator ~> macroArgument) ~
      (separator ~> macroArgument) ~
      (separator ~> instruction)
      ~ comment ^^ { case cond ~ m1 ~ m2 ~ i ~ c => IifDirectiveIdentical(cond, m1, m2, i, c) }
  private def iifDirective =
    exact(".IIF") ~> (iifDirectiveBlank | iifDirectiveCompare | iifDirectiveDefined | iifDirectiveIdentical)
  private def includeDirective = exact(".INCLUDE") ~> delimitedString ~ comment ^^ { case s ~ c => IncludeDirective(s, c) }
  private def libraryDirective = exact(".LIBRARY") ~> delimitedString ~ comment ^^ { case s ~ c => LibraryDirective(s, c) }
  private def limitDirective   = exact(".LIMIT") ~> comment ^^ { LimitDirective.apply }
  private def listDirective    = exact(".LIST") ~> opt(symbol) ~ comment ^^ { case s ~ c => ListDirective(s, c) }
  private def nlistDirective   = exact(".NLIST") ~> opt(symbol) ~ comment ^^ { case s ~ c => NListDirective(s, c) }
  private def noCrossDirective = exact(".NOCROSS") ~> repsep(symbol, separator) ~ comment ^^ { case ss ~ c =>
    NoCrossDirective(ss, c)
  }
  private def oddDirective = exact(".ODD") ~> comment ^^ { OddDirective.apply }
  private def packedDirective = exact(".PACKED") ~> digits ~ opt(separator ~> symbol) ~ comment ^^ { case d ~ s ~ c =>
    PackedDirective(d, s, c)
  }
  private def pageDirective = exact(".PAGE") ~> comment ^^ { PageDirective.apply }
  private def psectArg = ("RO" | "RW" | "I" | "D" | "GBL" | "LCL" | "ABS" | "REL" | "CON" | "OVR" | "SAV" | "NOSAV") ^^ { identity }
  private def psectDirective = exact(".PSECT") ~> rad50Symbol ~ (separator ~> rep1sep(psectArg, separator)) ~ comment ^^ {
    case n ~ pas ~ c => PSectDirective(n, pas, c)
  }
  private def rad50Directive = exact(".RAD50") ~> delimitedRad50String ~ comment ^^ { case s ~ c => Rad50Directive(s, c) }
  private def radixDirective = exact(".RADIX") ~> opt("2" | "8" | "10") ~ comment ^^ { case r ~ c =>
    RadixDirective(r.getOrElse(""), c)
  }
  private def remDirective     = exact(".REM") ~> anyCharacter ^^ { RemDirective.apply }
  private def restoreDirective = exact(".RESTORE") ~> comment ^^ { RestoreDirective.apply }
  private def saveDirective    = exact(".SAVE") ~> comment ^^ { SaveDirective.apply }
  private def sbttlDirective   = exact(".SBTTL") ~> anyString ^^ { SbttlDirective.apply }
  private def titleDirective   = exact(".TITLE") ~> anyString ^^ { TitleDirective.apply }
  private def weakDirective    = exact(".WEAK") ~> rep1sep(symbol, separator) ~ comment ^^ { case ss ~ c => WeakDirective(ss, c) }
  private def wordDirective = exact(".WORD") ~> rep1sep(expression, separator) ~ comment ^^ { case es ~ c =>
    WordDirective(es, c)
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
