package com.nigeleke.macro11

import com.nigeleke.macro11.ast.*
import com.nigeleke.macro11.parser.*
import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.*
import org.scalatestplus.scalacheck.*

class StatementParserSpec extends AnyWordSpec with ScalaCheckPropertyChecks with Matchers:

  object ParserUnderTest extends StatementParser
  import ParserUnderTest.*

  "The Statement parser" should {

    import com.nigeleke.macro11.Generators.*

    def parseAndCheckResult(s: String, expectedStatement: Statement) =
      ParserUnderTest.parse(ParserUnderTest.statement, s) match
        case Success(result, _)  => result should be(expectedStatement)
        case Failure(message, _) => fail(message)
        case Error(error, _)     => fail(error)

    "decode statements" when {
      "-labels-instruction-comment - i.e. empty" in {
        parseAndCheckResult("", Statement(List.empty, None, Comment("")))
      }

      "-labels-instruction+comment" in {
        forAll(genComment) { c =>
          parseAndCheckResult(c, Statement(List.empty, None, Comment(c)))
        }
      }

      "-labels+instruction-comment" in {
        forAll(genInstruction) { i =>
          val expectedInstruction = Instruction(Instruction.Mnemonic.valueOf(i), Seq.empty)
          parseAndCheckResult(i, Statement(List.empty, Option(expectedInstruction), Comment("")))
        }
      }

      "-labels+instruction+comment" in {
        forAll(genInstruction, genComment) { (i, c) =>
          val expectedInstruction = Instruction(Instruction.Mnemonic.valueOf(i), Seq.empty)
          parseAndCheckResult(s"$i $c", Statement(List.empty, Option(expectedInstruction), Comment(c)))
        }
      }

      "+labels-instruction-comment" in {
        forAll(genLabelList) { ls =>
          val expectedLabels = ls.map(Label.from)
          val lsString       = ls.mkString(" ")
          parseAndCheckResult(lsString, Statement(expectedLabels, None, Comment("")))
        }
      }

      "+labels-instruction+comment" in {
        forAll(genLabelList, genComment) { (ls, c) =>
          val expectedLabels = ls.map(Label.from)
          val lsString       = ls.mkString(" ")
          parseAndCheckResult(s"$lsString $c", Statement(expectedLabels, None, Comment(c)))
        }
      }

      "+labels+instruction-comment" in {
        forAll(genLabelList, genInstruction) { (ls, i) =>
          val expectedLabels      = ls.map(Label.from)
          val expectedInstruction = Instruction(Instruction.Mnemonic.valueOf(i), Seq.empty)
          val lsString            = ls.mkString(" ")
          parseAndCheckResult(s"$lsString $i", Statement(expectedLabels, Option(expectedInstruction), Comment("")))
        }
      }

      "+labels+instruction+comment" in {
        forAll(genLabelList, genInstruction, genComment) { (ls, i, c) =>
          val expectedLabels      = ls.map(Label.from)
          val expectedInstruction = Instruction(Instruction.Mnemonic.valueOf(i), Seq.empty)
          val lsString            = ls.mkString(" ")
          parseAndCheckResult(s"$lsString $i $c", Statement(expectedLabels, Option(expectedInstruction), Comment(c)))
        }
      }
    }
  }
