package com.nigeleke.macro11

import com.nigeleke.macro11.ast.*
import com.nigeleke.macro11.parser.*
import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.*
import org.scalatestplus.scalacheck.*

class DirectiveParserSpec extends AnyWordSpec with ScalaCheckDrivenPropertyChecks with Matchers:

  object ParserUnderTest extends DirectiveParser with InstructionParser
  import ParserUnderTest.*

  "The DirectiveParser" should {

    given noShrink[T]: Shrink[T] = Shrink[T](_ => Stream.empty)
    import Generators.*

    def parseAndCheckResult(i: String, expectedDirective: Directive) =
      ParserUnderTest.parse(ParserUnderTest.directive, i) match
        case Success(result, _)  => result should be(expectedDirective)
        case Failure(message, _) => fail(message)
        case Error(error, _)     => fail(error)

    def testDelimitedStringDirective(
        directive: String,
        genContent: Gen[String],
        expectedDirective: (String, Option[Comment]) => Directive
    ) =
      "simple string" in {
        forAll(genContent, genMaybeComment) { (s, maybeC) =>
          val c               = maybeC.getOrElse("")
          val expectedComment = maybeC.map(Comment(_))
          parseAndCheckResult(s"$directive $s $c", expectedDirective(s, expectedComment))
        }
      }
      "embedded expressions" ignore {}
      "delimited by semi-colon" ignore {}

    def testFloatingPointStorageDirective(
        directive: String,
        expectedDirective: (List[String], Option[Comment]) => Directive
    ) = testSingleListDirective(directive, genFloatList, expectedDirective)

    def testSingleExpressionDirective[T](
        directive: String,
        genExpression: Gen[T],
        expectedDirective: (T, Option[Comment]) => Directive
    ) =
      forAll(genExpression, genMaybeComment) { (s, maybeC) =>
        val c = maybeC.map(c => s" $c").getOrElse("")
        parseAndCheckResult(s"$directive $s$c", expectedDirective(s, maybeC.map(Comment(_))))
      }

    def testSingleOptionalExpressionDirective(
        directive: String,
        genMaybeExpression: Gen[Option[String]],
        expectedDirective: (Option[String], Option[Comment]) => Directive
    ) =
      forAll(genMaybeExpression, genMaybeComment) { (maybeS, maybeC) =>
        val s = maybeS.getOrElse("")
        val c = maybeC.map(c => s" $c").getOrElse("")
        parseAndCheckResult(s"$directive $s$c", expectedDirective(maybeS, maybeC.map(Comment(_))))
      }

    def testSingleListDirective(
        directive: String,
        genList: Gen[List[String]],
        expectedDirective: (List[String], Option[Comment]) => Directive
    ) =
      forAll(genList, genMaybeComment) { (items, maybeComment) =>
        val itemsAsString = items.mkString(", ")
        val comment       = maybeComment.getOrElse("")
        parseAndCheckResult(s"$directive $itemsAsString$comment", expectedDirective(items, maybeComment.map(Comment(_))))
      }

    def testNoParametersDirective(directive: String, expectedDirective: Option[Comment] => Directive) =
      forAll(genMaybeComment) { maybeC =>
        val c = maybeC.map(c => s" $c").getOrElse("")
        parseAndCheckResult(s"$directive$c", expectedDirective(maybeC.map(Comment(_))))
      }

    extension (s: String)
      def trimmedMacroArgument: String =
        require(s.startsWith("<"))
        require(s.endsWith(">"))
        s.substring(1, s.length - 1)

    "parse .ASCII directive" when {
      testDelimitedStringDirective(".ASCII", genSimpleDelimitedString, AsciiDirective(_, _))
    }

    "parse .ASCIZ directive" when {
      testDelimitedStringDirective(".ASCIZ", genSimpleDelimitedString, AscizDirective(_, _))
    }

    "parse .ASECT directive" in {
      testNoParametersDirective(".ASECT", ASectDirective(_))
    }

    "parse .BLKB directive" in {
      testSingleExpressionDirective(".BLKB", genNumericExpression, BlkbDirective(_, _))
    }

    "parse .BLKW directive" in {
      testSingleExpressionDirective(".BLKW", genNumericExpression, BlkwDirective(_, _))
    }

    "parse .BYTE directive" in {
      testSingleListDirective(".BYTE", genExpressionList, ByteDirective(_, _))
    }

    "parse .CROSS directive" in {
      testSingleListDirective(".CROSS", genSymbolsList, CrossDirective(_, _))
    }

    "parse .CSECT directive" in {
      testSingleExpressionDirective(".CSECT", genRad50Symbol, CSectDirective(_, _))
    }

    "parse .DSABL directive" in {
      testSingleExpressionDirective(".DSABL", genDsablEnablArgument, (a, c) => DsablDirective(EnablDsablArgument.valueOf(a), c))
    }

    "parse .ENABL directive" in {
      testSingleExpressionDirective(".ENABL", genDsablEnablArgument, (a, c) => EnablDirective(EnablDsablArgument.valueOf(a), c))
    }

    "parse .END directive" in {
      testSingleExpressionDirective(".END", genSymbol, EndDirective(_, _))
    }

    "parse .ENDC directive" in {
      testNoParametersDirective(".ENDC", EndcDirective(_))
    }

    "parse .EVEN directive" in {
      testNoParametersDirective(".EVEN", EvenDirective(_))
    }

    "parse .FLT2 directive" in {
      testFloatingPointStorageDirective(".FLT2", Flt2Directive(_, _))
    }

    "parse .FLT4 directive" in {
      testFloatingPointStorageDirective(".FLT4", Flt4Directive(_, _))
    }

    "parse .GLOBL directive" in {
      testSingleListDirective(".GLOBAL", genSymbolsList, GlobalDirective(_, _))
    }

    "parse .IDENT directive" when {
      testDelimitedStringDirective(".IDENT", genRad50DelimitedString, IdentDirective(_, _))
    }

    "parse .IF directive" when {
      "condition is oneOf EQ NE LT LE GE GT" in {
        val genCondition = Gen.oneOf("EQ", "NE", "LT", "LE", "GE", "GT")
        forAll(genCondition, genExpression, genMaybeComment) { (cond, e, maybeC) =>
          val c = maybeC.getOrElse("")
          parseAndCheckResult(s".IF $cond, $e $c", IfDirective(cond, List(e), maybeC.map(Comment(_))))
        }
      }

      // TODO: Include logical separators & and !
      "condition is oneOf DF NDF" in {
        val genCondition = Gen.oneOf("DF", "NDF")
        forAll(genCondition, genSymbol, genMaybeComment) { (cond, s, maybeC) =>
          val c = maybeC.getOrElse("")
          parseAndCheckResult(s".IF $cond, $s $c", IfDirective(cond, List(s), maybeC.map(Comment(_))))
        }
      }

      "condition is oneOf B NB" in {
        val genCondition = Gen.oneOf("B", "NB")
        forAll(genCondition, genMacroArgument, genMaybeComment) { (cond, ma, maybeC) =>
          val c = maybeC.getOrElse("")
          parseAndCheckResult(s".IF $cond, $ma $c", IfDirective(cond, List(ma.trimmedMacroArgument), maybeC.map(Comment(_))))
        }
      }

      "condition is oneOf IDN DIF" in {
        val genCondition = Gen.oneOf("IDN", "DIF")
        forAll(genCondition, genMacroArgument, genMacroArgument, genMaybeComment) { (cond, ma1, ma2, maybeC) =>
          val c = maybeC.getOrElse("")
          parseAndCheckResult(
            s".IF $cond, $ma1, $ma2 $c",
            IfDirective(cond, List(ma1.trimmedMacroArgument, ma2.trimmedMacroArgument), maybeC.map(Comment(_)))
          )
        }
      }
    }

    "parse .IFF directive" in {
      testNoParametersDirective(".IFF", IffDirective(_))
    }

    "parse .IFT directive" in {
      testNoParametersDirective(".IFT", IftDirective(_))
    }

    "parse .IFTF directive" in {
      testNoParametersDirective(".IFTF", IftfDirective(_))
    }

    "parse .IIF directive" when {

      "condition is oneOf EQ NE LT LE GE GT" in {
        val genCondition = Gen.oneOf("EQ", "NE", "LT", "LE", "GE", "GT")
        forAll(genCondition, genExpression, genInstruction, genMaybeComment) { (cond, e, i, maybeC) =>
          val expectedInstruction = ParserUnderTest.parse(ParserUnderTest.instruction, i).get
          val c                   = maybeC.getOrElse("")
          parseAndCheckResult(s".IIF $cond, $e, $i $c", IifDirective(cond, List(e), expectedInstruction, maybeC.map(Comment(_))))
        }
      }

      // TODO: Include logical separators & and !
      "condition is oneOf DF NDF" in {
        val genCondition = Gen.oneOf("DF", "NDF")
        forAll(genCondition, genSymbol, genInstruction, genMaybeComment) { (cond, s, i, maybeC) =>
          val expectedInstruction = ParserUnderTest.parse(ParserUnderTest.instruction, i).get
          val c                   = maybeC.getOrElse("")
          parseAndCheckResult(s".IIF $cond, $s, $i $c", IifDirective(cond, List(s), expectedInstruction, maybeC.map(Comment(_))))
        }
      }

      "condition is oneOf B NB" in {
        val genCondition = Gen.oneOf("B", "NB")
        forAll(genCondition, genMacroArgument, genInstruction, genMaybeComment) { (cond, ma, i, maybeC) =>
          val expectedInstruction = ParserUnderTest.parse(ParserUnderTest.instruction, i).get
          val c                   = maybeC.getOrElse("")
          parseAndCheckResult(
            s".IIF $cond, $ma, $i $c",
            IifDirective(cond, List(ma.trimmedMacroArgument), expectedInstruction, maybeC.map(Comment(_)))
          )
        }
      }

      "condition is oneOf IDN DIF" in {
        val genCondition = Gen.oneOf("IDN", "DIF")
        forAll(genCondition, genMacroArgument, genMacroArgument, genInstruction, genMaybeComment) { (cond, ma1, ma2, i, maybeC) =>
          val expectedInstruction = ParserUnderTest.parse(ParserUnderTest.instruction, i).get
          val c                   = maybeC.getOrElse("")
          parseAndCheckResult(
            s".IIF $cond, $ma1, $ma2, $i $c",
            IifDirective(
              cond,
              List(ma1.trimmedMacroArgument, ma2.trimmedMacroArgument),
              expectedInstruction,
              maybeC.map(Comment(_))
            )
          )
        }
      }
    }

    "parse .INCLUDE directive" when {
      testDelimitedStringDirective(".LIBRARY", genSimpleDelimitedString, LibraryDirective(_, _))
    }

    "parse .LIBRARY directive" when {
      testDelimitedStringDirective(".INCLUDE", genSimpleDelimitedString, IncludeDirective(_, _))
    }

    "parse .LIMIT directive" in {
      testNoParametersDirective(".LIMIT", LimitDirective(_))
    }

    "parse .LIST directive" in {
      testSingleOptionalExpressionDirective(".LIST", genMaybeSymbol, ListDirective(_, _))
    }

    "parse .MACRO directive" when {}

    "parse .NLIST directive" in {
      testSingleOptionalExpressionDirective(".NLIST", genMaybeSymbol, NListDirective(_, _))
    }

    "parse .NOCROSS directive" in {
      testSingleListDirective(".NOCROSS", genSymbolsList, NoCrossDirective(_, _))
    }

    "parse .ODD directive" in {
      testNoParametersDirective(".ODD", OddDirective(_))
    }

    "parse .PACKED directive" in {
      forAll(genDecimalString, genMaybeSymbol, genMaybeComment) { (d, maybeS, maybeC) =>
        val s = maybeS.map(s => s", $s").getOrElse("")
        val c = maybeC.map(c => s" $c").getOrElse("")
        parseAndCheckResult(s".PACKED $d$s$c", PackedDirective(d, maybeS, maybeC.map(Comment(_))))
      }
    }

    "parse .PAGE directive" in {
      testNoParametersDirective(".PAGE", PageDirective(_))
    }

    "parse .PSECT directive" in {
      forAll(genRad50Symbol, genPSectArguments, genMaybeComment) { (rs, pas, maybeC) =>
        val pasString = pas.mkString(", ")
        val c         = maybeC.getOrElse("")
        parseAndCheckResult(s".PSECT $rs, $pasString$c", PSectDirective(rs, pas, maybeC.map(Comment(_))))
      }
    }

    "parse .RAD50 directive" when {
      testDelimitedStringDirective(".RAD50", genRad50DelimitedString, Rad50Directive(_, _))
    }

    "parse .RADIX directive" in {
      forAll(genMaybeRadix, genMaybeComment) { (maybeR, maybeC) =>
        val r = maybeR.getOrElse("")
        val c = maybeC.getOrElse("")
        parseAndCheckResult(s".RADIX $r$c", RadixDirective(r, maybeC.map(Comment(_))))
      }
    }

    "parse .REM directive" in {
      forAll(Gen.asciiPrintableChar.suchThat(_ != ' ')) { r =>
        parseAndCheckResult(s".REM $r", RemDirective(s"$r"))
      }
    }

    "parse .RESTORE directive" in {
      testNoParametersDirective(".RESTORE", RestoreDirective(_))
    }

    "parse .SAVE directive" in {
      testNoParametersDirective(".SAVE", SaveDirective(_))
    }

    "parse .SBTTL directive" in {
      forAll(Gen.asciiPrintableStr) { s =>
        val cleanedS = s.trim
        parseAndCheckResult(s".SBTTL $cleanedS", SbttlDirective(cleanedS))
      }
    }

    "parse .TITLE directive" in {
      forAll(Gen.asciiPrintableStr) { s =>
        val cleanedS = s.trim
        parseAndCheckResult(s".TITLE $cleanedS", TitleDirective(cleanedS))
      }
    }

    "parse .WEAK directive" in {
      testSingleListDirective(".WEAK", genSymbolsList, WeakDirective(_, _))
    }

    "parse .WORD directive" in {
      testSingleListDirective(".WORD", genExpressionList, WordDirective(_, _))
    }

  }
