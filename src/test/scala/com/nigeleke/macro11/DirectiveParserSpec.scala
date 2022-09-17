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
        expectedDirective: (String, Comment) => Directive
    ) =
      "simple string" in {
        forAll(genContent, genComment) { (content, comment) =>
          parseAndCheckResult(s"$directive $content $comment", expectedDirective(content, Comment(comment)))
        }
      }
      "embedded expressions" ignore {}
      "delimited by semi-colon" ignore {}

    def testFloatingPointStorageDirective(
        directive: String,
        expectedDirective: (List[BigDecimal], Comment) => Directive
    ) = testSingleListDirective(directive, genFloatList, BigDecimal(_), expectedDirective)

    def testSingleExpressionDirective[T](
        directive: String,
        genExpression: Gen[T],
        expectedDirective: (T, Comment) => Directive
    ) =
      forAll(genExpression, genComment) { (s, comment) =>
        parseAndCheckResult(s"$directive $s$comment", expectedDirective(s, Comment(comment)))
      }

    def testSingleOptionalExpressionDirective(
        directive: String,
        genMaybeExpression: Gen[Option[String]],
        expectedDirective: (Option[String], Comment) => Directive
    ) =
      forAll(genMaybeExpression, genComment) { (maybeS, comment) =>
        val s = maybeS.getOrElse("")
        parseAndCheckResult(s"$directive $s$comment", expectedDirective(maybeS, Comment(comment)))
      }

    def testSingleListDirective[T](
        directive: String,
        genItems: Gen[List[String]],
        toT: String => T,
        expectedDirective: (List[T], Comment) => Directive
    ) =
      forAll(genItems, genComment) { (items, comment) =>
        val itemsAsString = items.mkString(", ")
        val expectedItems = items.map(toT)
        println(s"Items: $items")
        println(s"Expct: $expectedItems")
        parseAndCheckResult(s"$directive $itemsAsString$comment", expectedDirective(expectedItems, Comment(comment)))
      }

    def testNoParametersDirective(directive: String, expectedDirective: Comment => Directive) =
      forAll(genComment) { comment =>
        parseAndCheckResult(s"$directive$comment", expectedDirective(Comment(comment)))
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
      testSingleListDirective(".BYTE", genExpressionList, identity, ByteDirective(_, _))
    }

    "parse .CROSS directive" in {
      testSingleListDirective(".CROSS", genSymbolsList, identity, CrossDirective(_, _))
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
      testSingleListDirective(".GLOBAL", genSymbolsList, identity, GlobalDirective(_, _))
    }

    "parse .IDENT directive" when {
      testDelimitedStringDirective(".IDENT", genRad50DelimitedString, IdentDirective(_, _))
    }

    "parse .IF directive" when {
      "condition is oneOf EQ NE LT LE GE GT" in {
        val genCondition = Gen.oneOf("EQ", "NE", "LT", "LE", "GE", "GT")
        forAll(genCondition, genExpression, genComment) { (cond, e, comment) =>
          parseAndCheckResult(s".IF $cond, $e $comment", IfDirective(cond, List(e), Comment(comment)))
        }
      }

      // TODO: Include logical separators & and !
      "condition is oneOf DF NDF" in {
        val genCondition = Gen.oneOf("DF", "NDF")
        forAll(genCondition, genSymbol, genComment) { (cond, s, comment) =>
          parseAndCheckResult(s".IF $cond, $s $comment", IfDirective(cond, List(s), Comment(comment)))
        }
      }

      "condition is oneOf B NB" in {
        val genCondition = Gen.oneOf("B", "NB")
        forAll(genCondition, genMacroArgument, genComment) { (cond, ma, comment) =>
          parseAndCheckResult(s".IF $cond, $ma $comment", IfDirective(cond, List(ma.trimmedMacroArgument), Comment(comment)))
        }
      }

      "condition is oneOf IDN DIF" in {
        val genCondition = Gen.oneOf("IDN", "DIF")
        forAll(genCondition, genMacroArgument, genMacroArgument, genComment) { (cond, ma1, ma2, comment) =>
          parseAndCheckResult(
            s".IF $cond, $ma1, $ma2 $comment",
            IfDirective(cond, List(ma1.trimmedMacroArgument, ma2.trimmedMacroArgument), Comment(comment))
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
        forAll(genCondition, genExpression, genInstruction, genComment) { (cond, e, i, comment) =>
          val expectedInstruction = ParserUnderTest.parse(ParserUnderTest.instruction, i).get
          parseAndCheckResult(s".IIF $cond, $e, $i $comment", IifDirective(cond, List(e), expectedInstruction, Comment(comment)))
        }
      }

      // TODO: Include logical separators & and !
      "condition is oneOf DF NDF" in {
        val genCondition = Gen.oneOf("DF", "NDF")
        forAll(genCondition, genSymbol, genInstruction, genComment) { (cond, s, i, comment) =>
          val expectedInstruction = ParserUnderTest.parse(ParserUnderTest.instruction, i).get
          parseAndCheckResult(s".IIF $cond, $s, $i $comment", IifDirective(cond, List(s), expectedInstruction, Comment(comment)))
        }
      }

      "condition is oneOf B NB" in {
        val genCondition = Gen.oneOf("B", "NB")
        forAll(genCondition, genMacroArgument, genInstruction, genComment) { (cond, ma, i, comment) =>
          val expectedInstruction = ParserUnderTest.parse(ParserUnderTest.instruction, i).get
          parseAndCheckResult(
            s".IIF $cond, $ma, $i $comment",
            IifDirective(cond, List(ma.trimmedMacroArgument), expectedInstruction, Comment(comment))
          )
        }
      }

      "condition is oneOf IDN DIF" in {
        val genCondition = Gen.oneOf("IDN", "DIF")
        forAll(genCondition, genMacroArgument, genMacroArgument, genInstruction, genComment) { (cond, ma1, ma2, i, comment) =>
          val expectedInstruction = ParserUnderTest.parse(ParserUnderTest.instruction, i).get
          parseAndCheckResult(
            s".IIF $cond, $ma1, $ma2, $i $comment",
            IifDirective(
              cond,
              List(ma1.trimmedMacroArgument, ma2.trimmedMacroArgument),
              expectedInstruction,
              Comment(comment)
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
      testSingleListDirective(".NOCROSS", genSymbolsList, identity, NoCrossDirective(_, _))
    }

    "parse .ODD directive" in {
      testNoParametersDirective(".ODD", OddDirective(_))
    }

    "parse .PACKED directive" in {
      forAll(genDecimalString, genMaybeSymbol, genComment) { (d, maybeS, comment) =>
        val s = maybeS.map(s => s", $s").getOrElse("")
        parseAndCheckResult(s".PACKED $d$s$comment", PackedDirective(d, maybeS, Comment(comment)))
      }
    }

    "parse .PAGE directive" in {
      testNoParametersDirective(".PAGE", PageDirective(_))
    }

    "parse .PSECT directive" in {
      forAll(genRad50Symbol, genPSectArguments, genComment) { (rs, pas, comment) =>
        val pasString = pas.mkString(", ")
        parseAndCheckResult(s".PSECT $rs, $pasString$comment", PSectDirective(rs, pas, Comment(comment)))
      }
    }

    "parse .RAD50 directive" when {
      testDelimitedStringDirective(".RAD50", genRad50DelimitedString, Rad50Directive(_, _))
    }

    "parse .RADIX directive" in {
      forAll(genMaybeRadix, genComment) { (maybeR, comment) =>
        val r = maybeR.getOrElse("")
        parseAndCheckResult(s".RADIX $r$comment", RadixDirective(r, Comment(comment)))
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
      testSingleListDirective(".WEAK", genSymbolsList, identity, WeakDirective(_, _))
    }

    "parse .WORD directive" in {
      testSingleListDirective(".WORD", genExpressionList, identity, WordDirective(_, _))
    }

  }
