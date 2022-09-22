package com.nigeleke.macro11

import com.nigeleke.macro11.ast.*
import com.nigeleke.macro11.parser.*

import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.*
import org.scalatestplus.scalacheck.*

class StatementParserSpec extends AnyWordSpec with ScalaCheckDrivenPropertyChecks with Matchers:

  object ParserUnderTest extends StatementParser
  import ParserUnderTest.*

  "The Statement parser" should {

    given Shrink[String] = Shrink(s => Stream.empty)
    import Generators.*

    def parseAndCheckResult(s: String, expectedStatement: Statement) =
      ParserUnderTest.parse(ParserUnderTest.statement, s) match
        case Success(result, _)  => result should be(expectedStatement)
        case Failure(message, _) => fail(message)
        case Error(error, _)     => fail(error)

    extension (s: String)
      def toLabel: Label =
        if s.endsWith("::")
        then GlobalLabel(s.stripTrailing(":"))
        else LocalLabel(s.stripTrailing(":"))

      def stripTrailing(tail: String): String =
        if s.endsWith(tail)
        then s.take(s.length - tail.length).stripTrailing(tail)
        else s

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
          parseAndCheckResult(i, Statement(List.empty, Some(expectedInstruction), Comment("")))
        }
      }

      "-labels+instruction+comment" in {
        forAll(genInstruction, genComment) { (i, c) =>
          val expectedInstruction = Instruction(Instruction.Mnemonic.valueOf(i), Seq.empty)
          parseAndCheckResult(s"$i $c", Statement(List.empty, Some(expectedInstruction), Comment(c)))
        }
      }

      "+labels-instruction-comment" in {
        forAll(genLabelList) { ls =>
          val expectedLabels = ls.map(_.toLabel)
          val lsString       = ls.mkString(" ")
          parseAndCheckResult(lsString, Statement(expectedLabels, None, Comment("")))
        }
      }

      "+labels-instruction+comment" in {
        forAll(genLabelList, genComment) { (ls, c) =>
          val expectedLabels = ls.map(_.toLabel)
          val lsString       = ls.mkString(" ")
          parseAndCheckResult(s"$lsString $c", Statement(expectedLabels, None, Comment(c)))
        }
      }

      "+labels+instruction-comment" in {
        forAll(genLabelList, genInstruction) { (ls, i) =>
          val expectedLabels      = ls.map(_.toLabel)
          val expectedInstruction = Instruction(Instruction.Mnemonic.valueOf(i), Seq.empty)
          val lsString            = ls.mkString(" ")
          parseAndCheckResult(s"$lsString $i", Statement(expectedLabels, Some(expectedInstruction), Comment("")))
        }
      }

      "+labels+instruction+comment" in {
        forAll(genLabelList, genInstruction, genComment) { (ls, i, c) =>
          val expectedLabels      = ls.map(_.toLabel)
          val expectedInstruction = Instruction(Instruction.Mnemonic.valueOf(i), Seq.empty)
          val lsString            = ls.mkString(" ")
          parseAndCheckResult(s"$lsString $i $c", Statement(expectedLabels, Some(expectedInstruction), Comment(c)))
        }
      }
    }
  }
