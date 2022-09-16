package com.nigeleke.macro11.parser

import com.nigeleke.macro11.ast.*
import com.nigeleke.macro11.ast.Instruction.Mnemonic
import com.nigeleke.macro11.ast.Instruction.OperandRole
import com.nigeleke.macro11.parser.*

import scala.util.parsing.combinator.*

trait DirectiveParser extends InstructionParser with UtilityParser:

  private val delimitedString      = """(.)((?!\1).)*(\1)""".r ^^ { identity }
  private val delimitedRad50String = """(.)((?!\1)[A-Z0-9$. ])*(\1)""".r ^^ { identity }
  private val double               = """\d*(\.\d*)?""".r ^^ { identity }
  private val digits               = """\d+""".r ^^ { identity }
  private val rad50Symbol          = """[A-Z0-9$.]*""".r ^^ { identity }
  private def word(needed: String) = """\.\w+""".r >> { parsed =>
    if needed.equals(parsed) then success(parsed) else err(s"Found $parsed, not $needed")
  }

  private def asciiDirective = word(".ASCII") ~> delimitedString ~ opt(comment) ^^ { case s ~ c => AsciiDirective(s, c) }
  private def ascizDirective = word(".ASCIZ") ~> delimitedString ~ opt(comment) ^^ { case s ~ c => AscizDirective(s, c) }
  private def asectDirective = word(".ASECT") ~> opt(comment) ^^ { ASectDirective(_) }
  private def blkbDirective  = word(".BLKB") ~> numericExpression ~ opt(comment) ^^ { case e ~ c => BlkbDirective(e, c) }
  private def blkwDirective  = word(".BLKW") ~> numericExpression ~ opt(comment) ^^ { case e ~ c => BlkwDirective(e, c) }
  private def byteDirective = word(".BYTE") ~> rep1sep(expression, separator) ~ opt(comment) ^^ { case es ~ c =>
    ByteDirective(es, c)
  }
  private def crossDirective = word(".CROSS") ~> repsep(symbol, separator) ~ opt(comment) ^^ { case ss ~ c =>
    CrossDirective(ss, c)
  }
  private def csectDirective = word(".CSECT") ~> rad50Symbol ~ opt(comment) ^^ { case n ~ c => CSectDirective(n, c) }
  private def enablDsablArgument =
    val first :: rest = EnablDsablArgument.values.toList.map(_.toString ^^ identity): @unchecked
    rest.foldLeft(first)((es, e) => (es ||| e)) ^^ { EnablDsablArgument.valueOf(_) }
  private def dsablDirective = word(".DSABL") ~> enablDsablArgument ~ opt(comment) ^^ { case a ~ c => DsablDirective(a, c) }
  private def enablDirective = word(".ENABL") ~> enablDsablArgument ~ opt(comment) ^^ { case a ~ c => EnablDirective(a, c) }
  private def endDirective   = word(".END") ~> expression ~ opt(comment) ^^ { case e ~ c => EndDirective(e, c) }
  private def endcDirective  = word(".ENDC") ~> opt(comment) ^^ { EndcDirective(_) }
  private def evenDirective  = word(".EVEN") ~> opt(comment) ^^ { EvenDirective(_) }
  private def flt2Directive  = word(".FLT2") ~> repsep(double, separator) ~ opt(comment) ^^ { case fs ~ c => Flt2Directive(fs, c) }
  private def flt4Directive  = word(".FLT4") ~> repsep(double, separator) ~ opt(comment) ^^ { case fs ~ c => Flt4Directive(fs, c) }
  private def globalDirective = word(".GLOBAL") ~> rep1sep(symbol, separator) ~ opt(comment) ^^ { case ss ~ c =>
    GlobalDirective(ss, c)
  }
  private def identDirective = word(".IDENT") ~> delimitedString ~ opt(comment) ^^ { case s ~ c => IdentDirective(s, c) }
  private def ifDirectiveCompare =
    ("EQ" | "NE" | "LT" | "LE" | "GE" | "GT") ~ (separator ~> expression) ~ opt(comment) ^^ { case cond ~ e ~ c =>
      IfDirective(cond, List(e), c)
    }
  private def ifDirectiveDefined = ("DF" | "NDF") ~ (separator ~> symbol) ~ opt(comment) ^^ { case cond ~ s ~ c =>
    IfDirective(cond, List(s), c)
  }
  private def ifDirectiveMacroArg = ("B" | "NB") ~ (separator ~> macroArgument) ~ opt(comment) ^^ { case cond ~ m ~ c =>
    IfDirective(cond, List(m), c)
  }
  private def ifDirectiveMacroArgs =
    ("IDN" | "DIF") ~ (separator ~> macroArgument) ~ (separator ~> macroArgument) ~ opt(comment) ^^ { case cond ~ m1 ~ m2 ~ c =>
      IfDirective(cond, List(m1, m2), c)
    }
  private def ifDirective   = word(".IF") ~> (ifDirectiveCompare | ifDirectiveDefined | ifDirectiveMacroArg | ifDirectiveMacroArgs)
  private def iffDirective  = word(".IFF") ~> opt(comment) ^^ { IffDirective(_) }
  private def iftDirective  = word(".IFT") ~> opt(comment) ^^ { IftDirective(_) }
  private def iftfDirective = word(".IFTF") ~> opt(comment) ^^ { IftfDirective(_) }
  private def iifDirectiveCompare =
    ("EQ" | "NE" | "LT" | "LE" | "GE" | "GT") ~ (separator ~> expression) ~ (separator ~> instruction) ~ opt(comment) ^^ {
      case cond ~ e ~ i ~ c =>
        IifDirective(cond, List(e), i, c)
    }
  private def iifDirectiveDefined =
    ("DF" | "NDF") ~ (separator ~> symbol) ~ (separator ~> instruction) ~ opt(comment) ^^ { case cond ~ s ~ i ~ c =>
      IifDirective(cond, List(s), i, c)
    }
  private def iifDirectiveMacroArg =
    ("B" | "NB") ~ (separator ~> macroArgument) ~ (separator ~> instruction) ~ opt(comment) ^^ { case cond ~ m ~ i ~ c =>
      IifDirective(cond, List(m), i, c)
    }
  private def iifDirectiveMacroArgs =
    ("IDN" | "DIF") ~ (separator ~> macroArgument) ~ (separator ~> macroArgument) ~ (separator ~> instruction) ~ opt(
      comment
    ) ^^ { case cond ~ m1 ~ m2 ~ i ~ c =>
      IifDirective(cond, List(m1, m2), i, c)
    }
  private def iifDirective =
    word(".IIF") ~> (iifDirectiveCompare | iifDirectiveDefined | iifDirectiveMacroArg | iifDirectiveMacroArgs)
  private def includeDirective = word(".INCLUDE") ~> delimitedString ~ opt(comment) ^^ { case s ~ c => IncludeDirective(s, c) }
  private def libraryDirective = word(".LIBRARY") ~> delimitedString ~ opt(comment) ^^ { case s ~ c => LibraryDirective(s, c) }
  private def limitDirective   = word(".LIMIT") ~> opt(comment) ^^ { LimitDirective(_) }
  private def listDirective    = word(".LIST") ~> opt(symbol) ~ opt(comment) ^^ { case s ~ c => ListDirective(s, c) }
  private def nlistDirective   = word(".NLIST") ~> opt(symbol) ~ opt(comment) ^^ { case s ~ c => NListDirective(s, c) }
  private def noCrossDirective = word(".NOCROSS") ~> repsep(symbol, separator) ~ opt(comment) ^^ { case ss ~ c =>
    NoCrossDirective(ss, c)
  }
  private def oddDirective = word(".ODD") ~> opt(comment) ^^ { OddDirective(_) }
  private def packedDirective = word(".PACKED") ~> digits ~ opt(separator ~> symbol) ~ opt(comment) ^^ { case d ~ s ~ c =>
    PackedDirective(d, s, c)
  }
  private def pageDirective = word(".PAGE") ~> opt(comment) ^^ { PageDirective(_) }
  private def psectArg = ("RO" | "RW" | "I" | "D" | "GBL" | "LCL" | "ABS" | "REL" | "CON" | "OVR" | "SAV" | "NOSAV") ^^ { identity }
  private def psectDirective = word(".PSECT") ~> rad50Symbol ~ (separator ~> rep1sep(psectArg, separator)) ~ opt(comment) ^^ {
    case n ~ pas ~ c => PSectDirective(n, pas, c)
  }
  private def rad50Directive = word(".RAD50") ~> delimitedRad50String ~ opt(comment) ^^ { case s ~ c => Rad50Directive(s, c) }
  private def radixDirective = word(".RADIX") ~> opt("2" | "8" | "10") ~ opt(comment) ^^ { case r ~ c =>
    RadixDirective(r.getOrElse(""), c)
  }
  private def remDirective     = word(".REM") ~> anyCharacter ^^ { RemDirective(_) }
  private def restoreDirective = word(".RESTORE") ~> opt(comment) ^^ { RestoreDirective(_) }
  private def saveDirective    = word(".SAVE") ~> opt(comment) ^^ { SaveDirective(_) }
  private def sbttlDirective   = word(".SBTTL") ~> anyString ^^ { SbttlDirective(_) }
  private def titleDirective   = word(".TITLE") ~> anyString ^^ { TitleDirective(_) }
  private def weakDirective = word(".WEAK") ~> rep1sep(symbol, separator) ~ opt(comment) ^^ { case ss ~ c => WeakDirective(ss, c) }
  private def wordDirective = word(".WORD") ~> rep1sep(expression, separator) ~ opt(comment) ^^ { case es ~ c =>
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
