package com.nigeleke.macro11

import com.nigeleke.macro11.ast.*
import com.nigeleke.macro11.ast.directives.*
import com.nigeleke.macro11.parser.*
import org.scalacheck.Gen
import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.*
import org.scalatestplus.scalacheck.*

class DirectiveParserSpec extends AnyWordSpec with ScalaCheckPropertyChecks with Matchers:

  object ParserUnderTest extends DirectiveParser with InstructionParser with UtilityParser
  import ParserUnderTest.*

  "The DirectiveParser" should {

    import com.nigeleke.macro11.Generators.*

    def parseAndCheckResult(i: String, expectedDirective: Directive) =
      ParserUnderTest.parse(ParserUnderTest.directive, i) match
        case Success(result, _)  => result should be(expectedDirective)
        case Failure(message, _) => fail(message)
        case Error(error, _)     => fail(error)

    def testDelimitedStringDirective(
        directive: String,
        genContent: Gen[String],
        expectedDirective: (String, Comment) => Directive
    ): Unit =
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
    ): Unit = testListParameterDirective(directive, genFloatList, BigDecimal(_), expectedDirective)

    def testParameterDirective[T](
        directive: String,
        genParameter: Gen[String],
        toT: String => T,
        expectedDirective: (T, Comment) => Directive
    ): Unit =
      forAll(genParameter, genComment) { (parameter, comment) =>
        parseAndCheckResult(s"$directive $parameter$comment", expectedDirective(toT(parameter), Comment(comment)))
      }

    def testOptionalParameterDirective[T](
        directive: String,
        genMaybeParameter: Gen[Option[T]],
        expectedDirective: (Option[T], Comment) => Directive
    ): Unit =
      forAll(genMaybeParameter, genComment) { (maybeParameter, comment) =>
        val parameter = maybeParameter.getOrElse("")
        parseAndCheckResult(s"$directive $parameter$comment", expectedDirective(maybeParameter, Comment(comment)))
      }

    def testListParameterDirective[T](
        directive: String,
        genParameters: Gen[List[String]],
        toT: String => T,
        expectedDirective: (List[T], Comment) => Directive
    ): Unit =
      forAll(genParameters, genComment) { (parameters, comment) =>
        val parametersAsString = parameters.mkString(", ")
        val expectedParameters = parameters.map(toT)
        parseAndCheckResult(s"$directive $parametersAsString$comment", expectedDirective(expectedParameters, Comment(comment)))
      }

    def testNoParametersDirective(directive: String, expectedDirective: Comment => Directive): Unit =
      forAll(genComment) { comment =>
        parseAndCheckResult(s"$directive$comment", expectedDirective(Comment(comment)))
      }

    val stringToExpression: String => Expression                 = ParserUnderTest.parse(ParserUnderTest.expression, _).get
    val stringToEnablDsablArgument: String => EnablDsablArgument = EnablDsablArgument.valueOf

    "parse .ASCII directive" when {
      testDelimitedStringDirective(".ASCII", genSimpleDelimitedString, (s, c) => ASCII(DelimitedString.from(s), c))
    }

    "parse .ASCIZ directive" when {
      testDelimitedStringDirective(".ASCIZ", genSimpleDelimitedString, (s, c) => ASCIZ(DelimitedString.from(s), c))
    }

    "parse .ASECT directive" in {
      testNoParametersDirective(".ASECT", ASECT.apply)
    }

    "parse .BLKB directive" in {
      testParameterDirective(".BLKB", genExpression, stringToExpression, BLKB.apply)
    }

    "parse .BLKW directive" in {
      testParameterDirective(".BLKW", genExpression, stringToExpression, BLKW.apply)
    }

    "parse .BYTE directive" in {
      testListParameterDirective(".BYTE", genExpressionList, stringToExpression, BYTE.apply)
    }

    "parse .CROSS directive" in {
      testListParameterDirective(".CROSS", genSymbolsList, identity, CROSS.apply)
    }

    "parse .CSECT directive" in {
      testParameterDirective(".CSECT", genRad50Symbol, identity, CSECT.apply)
    }

    "parse .DSABL directive" in {
      testParameterDirective(".DSABL", genDsablEnablArgument, stringToEnablDsablArgument, DSABL.apply)
    }

    "parse .ENABL directive" in {
      testParameterDirective(".ENABL", genDsablEnablArgument, stringToEnablDsablArgument, ENABL.apply)
    }

    "parse .END directive" in {
      val stringToExpression: String => Expression = ParserUnderTest.parse(ParserUnderTest.expression, _).get
      testOptionalParameterDirective(".END", genExpressionOption, (s, c) => END(s.map(stringToExpression), c))
    }

    "parse .ENDC directive" in {
      testNoParametersDirective(".ENDC", ENDC.apply)
    }

    "parse .EVEN directive" in {
      testNoParametersDirective(".EVEN", EVEN.apply)
    }

    "parse .FLT2 directive" in {
      testFloatingPointStorageDirective(".FLT2", FLT2.apply)
    }

    "parse .FLT4 directive" in {
      testFloatingPointStorageDirective(".FLT4", FLT4.apply)
    }

    "parse .GLOBL directive" in {
      testListParameterDirective(".GLOBL", genSymbolsList, identity, GLOBL.apply)
    }

    "parse .IDENT directive" when {
      testDelimitedStringDirective(".IDENT", genRad50DelimitedString, (s, c) => IDENT(DelimitedString.from(s), c))
    }

    "parse .IF directive" when {
      "condition is oneOf B NB" in {
        val genCondition = Gen.oneOf("B", "NB")
        forAll(genCondition, genMacroArgument, genComment) { (cond, ma, comment) =>
          val expectedArgument  = ParserUnderTest.parse(ParserUnderTest.macroArgument, ma).get
          val expectedDirective = IFBlank(cond, expectedArgument, Comment(comment))
          parseAndCheckResult(s".IF $cond, $ma $comment", expectedDirective)
        }
      }

      "condition is oneOf EQ NE LT LE GE GT" in {
        val genCondition = Gen.oneOf("EQ", "NE", "LT", "LE", "GE", "GT")
        forAll(genCondition, genExpression, genComment) { (cond, e, comment) =>
          val expectedExpression = ParserUnderTest.parse(ParserUnderTest.expression, e).get
          val expectedDirective  = IFCompare(cond, expectedExpression, Comment(comment))
          parseAndCheckResult(s".IF $cond, $e $comment", expectedDirective)
        }
      }

      // TODO: Include logical separators & and !
      "condition is oneOf DF NDF" in {
        val genCondition = Gen.oneOf("DF", "NDF")
        forAll(genCondition, genSymbol, genComment) { (cond, s, comment) =>
          val expectedDirective = IFDefined(cond, s, Comment(comment))
          parseAndCheckResult(s".IF $cond, $s $comment", expectedDirective)
        }
      }

      "condition is oneOf IDN DIF" in {
        val genCondition = Gen.oneOf("IDN", "DIF")
        forAll(genCondition, genMacroArgument, genMacroArgument, genComment) { (cond, ma1, ma2, comment) =>
          val expectedArgument1 = ParserUnderTest.parse(ParserUnderTest.macroArgument, ma1).get
          val expectedArgument2 = ParserUnderTest.parse(ParserUnderTest.macroArgument, ma2).get
          val expectedDirective = IFIdentical(cond, expectedArgument1, expectedArgument2, Comment(comment))
          parseAndCheckResult(s".IF $cond, $ma1, $ma2 $comment", expectedDirective)
        }
      }
    }

    "parse .IFF directive" in {
      testNoParametersDirective(".IFF", IFF.apply)
    }

    "parse .IFT directive" in {
      testNoParametersDirective(".IFT", IFT.apply)
    }

    "parse .IFTF directive" in {
      testNoParametersDirective(".IFTF", IFTF.apply)
    }

    "parse .IIF directive" when {

      "condition is oneOf B NB" in {
        val genCondition = Gen.oneOf("B", "NB")
        forAll(genCondition, genMacroArgument, genInstruction, genComment) { (cond, ma, i, comment) =>
          val expectedArgument    = ParserUnderTest.parse(ParserUnderTest.macroArgument, ma).get
          val expectedInstruction = ParserUnderTest.parse(ParserUnderTest.instruction, i).get
          val expectedDirective   = IIFBlank(cond, expectedArgument, expectedInstruction, Comment(comment))
          parseAndCheckResult(s".IIF $cond, $ma, $i $comment", expectedDirective)
        }
      }

      "condition is oneOf EQ NE LT LE GE GT" in {
        val genCondition = Gen.oneOf("EQ", "NE", "LT", "LE", "GE", "GT")
        forAll(genCondition, genExpression, genInstruction, genComment) { (cond, e, i, comment) =>
          val expectedExpression  = ParserUnderTest.parse(ParserUnderTest.expression, e).get
          val expectedInstruction = ParserUnderTest.parse(ParserUnderTest.instruction, i).get
          val expectedDirective   = IIFCompare(cond, expectedExpression, expectedInstruction, Comment(comment))
          parseAndCheckResult(s".IIF $cond, $e, $i $comment", expectedDirective)
        }
      }

      // TODO: Include logical separators & and !
      "condition is oneOf DF NDF" in {
        val genCondition = Gen.oneOf("DF", "NDF")
        forAll(genCondition, genSymbol, genInstruction, genComment) { (cond, s, i, comment) =>
          val expectedInstruction = ParserUnderTest.parse(ParserUnderTest.instruction, i).get
          val expectedDirective   = IIFDefined(cond, s, expectedInstruction, Comment(comment))
          parseAndCheckResult(s".IIF $cond, $s, $i $comment", expectedDirective)
        }
      }

      "condition is oneOf IDN DIF" in {
        val genCondition = Gen.oneOf("IDN", "DIF")
        forAll(genCondition, genMacroArgument, genMacroArgument, genInstruction, genComment) { (cond, ma1, ma2, i, comment) =>
          val expectedArgument1   = ParserUnderTest.parse(ParserUnderTest.macroArgument, ma1).get
          val expectedArgument2   = ParserUnderTest.parse(ParserUnderTest.macroArgument, ma2).get
          val expectedInstruction = ParserUnderTest.parse(ParserUnderTest.instruction, i).get
          val expectedDirective =
            IIFIdentical(cond, expectedArgument1, expectedArgument2, expectedInstruction, Comment(comment))
          parseAndCheckResult(s".IIF $cond, $ma1, $ma2, $i $comment", expectedDirective)
        }
      }
    }

    "parse .INCLUDE directive" when {
      testDelimitedStringDirective(".LIBRARY", genSimpleDelimitedString, (s, c) => LIBRARY(DelimitedString.from(s), c))
    }

    "parse .LIBRARY directive" when {
      testDelimitedStringDirective(".INCLUDE", genSimpleDelimitedString, (s, c) => INCLUDE(DelimitedString.from(s), c))
    }

    "parse .LIMIT directive" in {
      testNoParametersDirective(".LIMIT", LIMIT.apply)
    }

    "parse .LIST directive" in {
      testOptionalParameterDirective(".LIST", genSymbolOption, LIST.apply)
    }

    "parse .MACRO directive" when {}

    "parse .NLIST directive" in {
      testOptionalParameterDirective(".NLIST", genSymbolOption, NLIST.apply)
    }

    "parse .NOCROSS directive" in {
      testListParameterDirective(".NOCROSS", genSymbolsList, identity, NOCROSS.apply)
    }

    "parse .ODD directive" in {
      testNoParametersDirective(".ODD", ODD.apply)
    }

    "parse .PACKED directive" in {
      forAll(genDecimalString, genSymbolOption, genComment) { (d, maybeS, comment) =>
        val s = maybeS.fold("")(s => s", $s")
        parseAndCheckResult(s".PACKED $d$s$comment", PACKED(d, maybeS, Comment(comment)))
      }
    }

    "parse .PAGE directive" in {
      testNoParametersDirective(".PAGE", PAGE.apply)
    }

    "parse .PSECT directive" in {
      forAll(genRad50Symbol, genPSectArguments, genComment) { (rs, pas, comment) =>
        val pasString = pas.mkString(", ")
        parseAndCheckResult(s".PSECT $rs, $pasString$comment", PSECT(rs, pas, Comment(comment)))
      }
    }

    "parse .RAD50 directive" when {
      testDelimitedStringDirective(".RAD50", genRad50DelimitedString, (s, c) => RAD50(DelimitedString.from(s), c))
    }

    "parse .RADIX directive" in {
      forAll(genMaybeRadix, genComment) { (maybeR, comment) =>
        val r = maybeR.getOrElse("")
        parseAndCheckResult(s".RADIX $r$comment", RADIX(r, Comment(comment)))
      }
    }

    "parse .REM directive" in {
      forAll(Gen.asciiPrintableChar.suchThat(_ != ' ')) { r =>
        parseAndCheckResult(s".REM $r", REM(s"$r"))
      }
    }

    "parse .RESTORE directive" in {
      testNoParametersDirective(".RESTORE", RESTORE.apply)
    }

    "parse .SAVE directive" in {
      testNoParametersDirective(".SAVE", SAVE.apply)
    }

    "parse .SBTTL directive" in {
      forAll(Gen.asciiPrintableStr) { s =>
        val cleanedS = s.trim
        parseAndCheckResult(s".SBTTL $cleanedS", SBTTL(cleanedS))
      }
    }

    "parse .TITLE directive" in {
      forAll(Gen.asciiPrintableStr) { s =>
        val cleanedS = s.trim
        parseAndCheckResult(s".TITLE $cleanedS", TITLE(cleanedS))
      }
    }

    "parse .WEAK directive" in {
      testListParameterDirective(".WEAK", genSymbolsList, identity, WEAK.apply)
    }

    "parse .WORD directive" in {
      testListParameterDirective(".WORD", genExpressionList, stringToExpression, WORD.apply)
    }

  }
