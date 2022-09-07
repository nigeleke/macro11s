package com.nigeleke.macro11

import com.nigeleke.macro11.ast.*
import com.nigeleke.macro11.ast.Instruction.*
import com.nigeleke.macro11.parser.*

import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.*
import org.scalatestplus.scalacheck.*

class AstCodecSpec extends AnyWordSpec with ScalaCheckDrivenPropertyChecks with Matchers:

  import Macro11Parser.*

  given Shrink[String] = Shrink(s => Stream.empty)

  extension (s: String)
    def stripTrailing(tail: String): String =
      if s.endsWith(tail)
      then s.take(s.length - tail.length).stripTrailing(tail)
      else s

  val genSymbol =
    for symbol <- Gen
        .someOf(('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9') ++ "$.".toSeq)
        .suchThat(_.nonEmpty)
    yield String(symbol.toArray)

  val genLabel =
    for
      label  <- genSymbol
      suffix <- Gen.oneOf(Seq(":", "::"))
    yield s"$label$suffix"
  val genLabels = Gen.listOf(genLabel)

  val genRegister = Gen.oneOf((0 to 7).map(r => s"%$r") ++ (0 to 5).map(r => s"R$r") ++ Seq("SP", "PC"))

  // format: off
  val genOperand = for
    r <- genRegister
    s <- genSymbol
    operand <- Gen.oneOf(Seq(
      s"$r", s"@$r", s"($r)", s"($r)+", s"@($r)+", s"-($r)", s"@-($r)",
      s"$s($r)", s"@$s($r)", s"#$s", s"@#$s", s"$s", s"@$s"))
  yield operand
  // format: on

  val genSingleOperandInstruction =
    for
      instruction           <- Gen.oneOf(Instruction.singleOperandMnemonics.map(_.toString))
      addressingModeOperand <- genOperand
    yield s"$instruction $addressingModeOperand"

  val genDoubleOperandInstruction =
    for
      instruction        <- Gen.oneOf(Instruction.doubleOperandMnemonics.map(_.toString))
      sourceOperand      <- genOperand
      destinationOperand <- genOperand
    yield s"$instruction $sourceOperand, $destinationOperand"

  val genBranchInstruction =
    for
      instruction <- Gen.oneOf(Instruction.branchMnemonics.map(_.toString))
      address     <- genSymbol
    yield s"$instruction $address"

  val genSobInstruction =
    for
      register <- genRegister
      address  <- genSymbol
    yield s"SOB $register, $address"

  val genInstruction = Gen.oneOf(genSingleOperandInstruction, genDoubleOperandInstruction, genBranchInstruction, genSobInstruction)

  val genComment = Gen.oneOf(Seq("", "; Comment"))

  "The identifier codec" should {
    "decode a pure symbol" in {
      forAll(genSymbol) { s =>
        Macro11Parser.parse(symbol, s) match
          case Success(result, remainder) =>
            result should be(s)
            remainder.atEnd should be(true)
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }
    }

    "decode a symbol followed by whitespace" in {
      forAll(genSymbol) { s =>
        Macro11Parser.parse(symbol, s"$s ") match
          case Success(result, remainder) =>
            result should be(s)
            remainder.atEnd should be(false)
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }
    }
  }

  "The label codec" should {
    "decode a pure label" in {
      forAll(genLabel) { l =>
        val expectedSymbol = l.stripTrailing(":")
        Macro11Parser.parse(label, l) match
          case Success(result, remainder) =>
            result match
              case GlobalLabel(symbol) => symbol should be(expectedSymbol)
              case LocalLabel(symbol)  => symbol should be(expectedSymbol)
            remainder.atEnd should be(true)
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }
    }

    "decode a label followed by whitespace" in {
      forAll(genLabel) { l =>
        val expectedSymbol = l.stripTrailing(":")
        Macro11Parser.parse(label, s"$l ") match
          case Success(result, remainder) =>
            result match
              case GlobalLabel(symbol) => symbol should be(expectedSymbol)
              case LocalLabel(symbol)  => symbol should be(expectedSymbol)
            remainder.atEnd should be(false)
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }
    }
  }

  "The labels codec" should {
    "decode zero labels" in {
      Macro11Parser.parse(labels, "") match
        case Success(result, remainder) =>
          result should be(empty)
          remainder.atEnd should be(true)
        case Failure(message, _) => fail(message)
        case Error(error, _)     => fail(error)
    }

    "decode a single label" in {
      forAll(genLabel) { l =>
        val expectedSymbol = l.stripTrailing(":")
        Macro11Parser.parse(labels, l) match
          case Success(result, remainder) =>
            result.size should be(1)
            result.head.symbol should be(expectedSymbol)
            remainder.atEnd should be(true)
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }
    }

    "decode multiple labels (without separator)" in {
      forAll(genLabels) { ls =>
        val expectedSymbols = ls.map(_.stripTrailing(":"))
        val lsString        = ls.mkString("")
        Macro11Parser.parse(labels, lsString) match
          case Success(result, remainder) =>
            result.size should be(ls.size)
            result.map(_.symbol) should contain theSameElementsAs (expectedSymbols)
            remainder.atEnd should be(true)
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }
    }

    "decode multiple labels (with separator)" in {
      forAll(genLabels) { ls =>
        val expectedSymbols = ls.map(_.stripTrailing(":"))
        val lsString        = ls.mkString(" ")
        Macro11Parser.parse(labels, lsString) match
          case Success(result, remainder) =>
            result.size should be(ls.size)
            result.map(_.symbol) should contain theSameElementsAs (expectedSymbols)
            remainder.atEnd should be(true)
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }
    }
  }

  "The addressingModeOperand codec" should {
    "decode register mode operands" in {
      forAll(genRegister) { r =>
        Macro11Parser.parse(addressingModeOperand, r) match
          case Success(result, remainder) =>
            result should be(RegisterMode(r))
            remainder.atEnd should be(true)
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }
    }

    "decode deferred register mode operands - variant 1" in {
      // TODO: May be create a genRegisterExpression
      forAll(genRegister) { r =>
        Macro11Parser.parse(addressingModeOperand, s"@$r") match
          case Success(result, remainder) =>
            result should be(RegisterDeferredMode(r))
            remainder.atEnd should be(true)
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }
    }

    "decode deferred register mode operands - variant 2" in {
      // TODO: May be create a genRegisterExpression
      forAll(genRegister) { r =>
        Macro11Parser.parse(addressingModeOperand, s"($r)") match
          case Success(result, remainder) =>
            result should be(RegisterDeferredMode(r))
            remainder.atEnd should be(true)
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }
    }

    "decode auto increment mode operands" in {
      forAll(genRegister) { r =>
        Macro11Parser.parse(addressingModeOperand, s"($r)+") match
          case Success(result, remainder) =>
            result should be(AutoIncrementMode(r))
            remainder.atEnd should be(true)
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }
    }

    "decode deferred auto increment mode operands - variant 1" in {
      // TODO: May be create a genRegisterExpression
      forAll(genRegister) { r =>
        Macro11Parser.parse(addressingModeOperand, s"@($r)+") match
          case Success(result, remainder) =>
            result should be(AutoIncrementDeferredMode(r))
            remainder.atEnd should be(true)
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }
    }

    "decode auto decrement mode operands" in {
      forAll(genRegister) { r =>
        Macro11Parser.parse(addressingModeOperand, s"-($r)") match
          case Success(result, remainder) =>
            result should be(AutoDecrementMode(r))
            remainder.atEnd should be(true)
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }
    }

    "decode deferred auto decrement mode operands - variant 1" in {
      // TODO: May be create a genRegisterExpression
      forAll(genRegister) { r =>
        Macro11Parser.parse(addressingModeOperand, s"@-($r)") match
          case Success(result, remainder) =>
            result should be(AutoDecrementDeferredMode(r))
            remainder.atEnd should be(true)
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }
    }

    "decode index mode operands" in {
      forAll(genSymbol, genRegister) { (s, r) =>
        Macro11Parser.parse(addressingModeOperand, s"$s($r)") match
          case Success(result, remainder) =>
            result should be(IndexMode(s, r))
            remainder.atEnd should be(true)
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }
    }

    "decode index deferred mode operands" in {
      forAll(genSymbol, genRegister) { (s, r) =>
        Macro11Parser.parse(addressingModeOperand, s"@$s($r)") match
          case Success(result, remainder) =>
            result should be(IndexDeferredMode(s, r))
            remainder.atEnd should be(true)
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }
    }

    "decode immediate mode operands" in {
      forAll(genSymbol) { s =>
        Macro11Parser.parse(addressingModeOperand, s"#$s") match
          case Success(result, remainder) =>
            result should be(ImmediateMode(s))
            remainder.atEnd should be(true)
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }
    }

    "decode absolute mode operands" in {
      forAll(genSymbol) { s =>
        Macro11Parser.parse(addressingModeOperand, s"@#$s") match
          case Success(result, remainder) =>
            result should be(AbsoluteMode(s))
            remainder.atEnd should be(true)
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }
    }

    "decode relative mode operands" in {
      forAll(genSymbol) { s =>
        Macro11Parser.parse(addressingModeOperand, s) match
          case Success(result, remainder) =>
            result should be(RelativeMode(s))
            remainder.atEnd should be(true)
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }
    }

    "decode relative deferred mode operands" in {
      forAll(genSymbol) { s =>
        Macro11Parser.parse(addressingModeOperand, s"@$s") match
          case Success(result, remainder) =>
            result should be(RelativeDeferredMode(s))
            remainder.atEnd should be(true)
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }
    }
  }

  "The instruction mnemonics" should {
    "be fully used in instruction families" in {
      val knownDiscreteMnemonics = Set(Instruction.Mnemonic.SOB)
      val familySet = Instruction.singleOperandMnemonics.toSet ++
        Instruction.doubleOperandMnemonics.toSet ++
        Instruction.branchMnemonics.toSet ++
        knownDiscreteMnemonics
      val missing = Instruction.Mnemonic.values.toSet -- familySet
      missing should be(empty)
    }

    "not be duplicated across family definitions" in {
      val referenceCounts =
        (Instruction.singleOperandMnemonics ++ Instruction.doubleOperandMnemonics ++ Instruction.branchMnemonics)
          .groupMapReduce(identity)(_ => 1)(_ + _)
      val duplicated = referenceCounts.filter(_._2 > 1)
      duplicated should be(empty)
    }
  }

  "The instruction codec" should {
    "decode single addressingModeOperand instructions" in {
      forAll(genSingleOperandInstruction) { i =>
        val expectedMnemonic = i.split(' ').head
        Macro11Parser.parse(singleOperandInstruction, i) match
          case Success(result, remainder) =>
            result should be(a[SingleOperandInstruction])
            result.mnemonic should be(Instruction.Mnemonic.valueOf(expectedMnemonic))
            remainder.atEnd should be(true)
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }
    }

    "decode double addressingModeOperand instructions" in {
      forAll(genDoubleOperandInstruction) { i =>
        val expectedMnemonic = i.split(' ').head
        Macro11Parser.parse(doubleOperandInstruction, i) match
          case Success(result, remainder) =>
            result should be(a[DoubleOperandInstruction])
            result.mnemonic should be(Instruction.Mnemonic.valueOf(expectedMnemonic))
            remainder.atEnd should be(true)
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }
    }

    "decode branch instructions" in {
      forAll(genBranchInstruction) { i =>
        val expectedMnemonic = i.split(' ').head
        Macro11Parser.parse(branchInstruction, i) match
          case Success(result, remainder) =>
            result should be(a[BranchInstruction])
            result.mnemonic should be(Instruction.Mnemonic.valueOf(expectedMnemonic))
            remainder.atEnd should be(true)
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }
    }

    "decode miscellaneous instructions" when {
      "SubtractOneAndBranch" in {
        forAll(genSobInstruction) { i =>
          Macro11Parser.parse(sobInstruction, i) match
            case Success(result, remainder) =>
              result should be(a[SobInstruction])
              result.mnemonic should be(Instruction.Mnemonic.valueOf(Instruction.Mnemonic.SOB.toString))
              remainder.atEnd should be(true)
            case Failure(message, _) => fail(message)
            case Error(error, _)     => fail(error)
        }
      }
    }

    "decode all instructions" in {
      forAll(genInstruction) { i =>
        val expectedMnemonic = i.split(' ').head
        Macro11Parser.parse(instruction, i) match
          case Success(result, remainder) =>
            def confirmExpected(mnemonic: Mnemonic) = mnemonic.toString should be(expectedMnemonic)
            result match
              case SingleOperandInstruction(mnemonic, _)    => confirmExpected(mnemonic)
              case DoubleOperandInstruction(mnemonic, _, _) => confirmExpected(mnemonic)
              case BranchInstruction(mnemonic, _)           => confirmExpected(mnemonic)
              case i: SobInstruction                        => confirmExpected(i.mnemonic)
            remainder.atEnd should be(true)
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }
    }
  }

  ////    "decode statements" ignore {
  ////      forAll(genLabel, genInstruction, genComment) { (label, instruction, comment) =>
  ////        val line = s"$label$instruction$comment"
  ////        println(line)
  ////        if (!line.isEmpty)
  ////          val result = Macro11Codec.macro11Codec.decode(BitVector(line.getBytes)).require
  ////          val (maybeLabel, maybeInstruction, maybeComment) = result.value
  ////          val expectedMaybeLabel = label match
  ////            case s if s.isEmpty        => None
  ////            case s if s.endsWith("::") => Some(Label(true, label.stripTrailing(":")))
  ////            case s if s.endsWith(":")  => Some(Label(false, label.stripTrailing(":")))
  ////          maybeLabel should be(expectedMaybeLabel)
  ////          maybeInstruction match
  ////            case None if instruction.isEmpty => passed
  ////            case Some(instruction)           => passed
  ////            case other                       => fail(s"instruction mismatch: $other")
  //////          maybeComment should be(if comment.isEmpty then None else Some(Comment(comment.tail)))
  ////        passed
  ////      }
  ////    }
  //
  ////    "decode the instruction set" ignore {
  ////      "branch instruction" ignore {}
  ////      "jump and subroutine instruction" ignore {}
  ////      "trap instruction" ignore {}
  ////      "miscellaneous instruction" ignore {}
  ////      "condition code instruction" ignore {}
  ////    }
