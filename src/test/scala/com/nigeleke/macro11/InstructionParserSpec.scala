package com.nigeleke.macro11

import com.nigeleke.macro11.ast.*
import com.nigeleke.macro11.ast.Instruction.Mnemonic
import com.nigeleke.macro11.ast.Instruction.OperandRole
import com.nigeleke.macro11.parser.*
import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.*
import org.scalatestplus.scalacheck.*

class InstructionParserSpec extends AnyWordSpec with ScalaCheckDrivenPropertyChecks with Matchers:

  object ParserUnderTest extends InstructionParser
  import ParserUnderTest.*

  "The InstructionParser" should {

    given Shrink[String] = Shrink(_ => Stream.empty)
    import Generators.*

    def parseAndCheckResult(i: String, mnemonic: String, expectedParams: Seq[Operand]) =
      val expectedMnemonic = Mnemonic.valueOf(mnemonic)
      ParserUnderTest.parse(ParserUnderTest.instruction, i) match
        case Success(result, _)  => result should be(Instruction(expectedMnemonic, expectedParams))
        case Failure(message, _) => fail(message)
        case Error(error, _)     => fail(error)

    def parseOperand(parser: Parser[Operand], from: String) =
      ParserUnderTest.parse(parser, from) match
        case Success(result, _) => result
        case other              => throw IllegalArgumentException(other.toString)

    "parse instructions" when {

      "there are no operands" in {
        forAll(genNoOperandMnemonic) { i => parseAndCheckResult(i, i, Seq.empty) }
      }

      "there is a destination addressing mode operand" in {
        forAll(genDestinationOperandMnemonic, genAddressingModeOperand) { (i, ddd) =>
          val expectedDdd = parseOperand(ParserUnderTest.addressingModeOperand, ddd)
          parseAndCheckResult(s"$i $ddd", i, List(expectedDdd))
        }
      }

      "there are source and destination addressing operandParser operands" in {
        forAll(genSourceDestinationOperandMnemonic, genAddressingModeOperand, genAddressingModeOperand) { (i, sss, ddd) =>
          val expectedSss = parseOperand(ParserUnderTest.addressingModeOperand, sss)
          val expectedDdd = parseOperand(ParserUnderTest.addressingModeOperand, ddd)
          parseAndCheckResult(s"$i $sss, $ddd", i, List(expectedSss, expectedDdd))
        }
      }

      "there is an address offset operandParser" in {
        forAll(genAddressOffsetOperandMnemonic, genAddressOffsetOperand) { (i, a) =>
          val expectedA = parseOperand(ParserUnderTest.addressOffsetOperand, a)
          parseAndCheckResult(s"$i $a", i, List(expectedA))
        }
      }

      "there are register and address offset operands" in {
        forAll(genRegisterAddressOffsetOperandMnemonic, genRegister, genAddressOffsetOperand) { (i, r, a) =>
          val expectedR = parseOperand(ParserUnderTest.registerOperand, r)
          val expectedA = parseOperand(ParserUnderTest.addressOffsetOperand, a)
          parseAndCheckResult(s"$i $r, $a", i, List(expectedR, expectedA))
        }
      }

      "there are register and destination addressing mode operands" in {
        forAll(genRegisterDestinationOperandMnemonic, genRegister, genAddressingModeOperand) { (i, r, ddd) =>
          val expectedR   = parseOperand(ParserUnderTest.registerOperand, r)
          val expectedDdd = parseOperand(ParserUnderTest.addressingModeOperand, ddd)
          parseAndCheckResult(s"$i $r, $ddd", i, List(expectedR, expectedDdd))
        }
      }

      "there is a parameter count operand" in {
        forAll(genParameterCountOperandMnemonic, genSymbol) { (i, p) =>
          val expectedP = parseOperand(ParserUnderTest.parameterCountOperand, p)
          parseAndCheckResult(s"$i $p", i, Seq(expectedP))
        }
      }

      "there is a trap parameter operand" in {
        forAll(genTrapOperandMnemonic, genSymbol) { (i, tp) =>
          val expectedTp = parseOperand(ParserUnderTest.trapParameterOperand, tp)
          parseAndCheckResult(s"$i $tp", i, Seq(expectedTp))
        }
      }

      "there are register and source addressing mode operands" in {
        forAll(genRegisterSourceOperandMnemonic, genRegister, genAddressingModeOperand) { (i, r, sss) =>
          val expectedR   = parseOperand(ParserUnderTest.registerOperand, r)
          val expectedSss = parseOperand(ParserUnderTest.addressingModeOperand, sss)
          parseAndCheckResult(s"$i $r, $sss", i, List(expectedR, expectedSss))
        }
      }

      "there is a register operand" in {
        forAll(genRegisterOperandMnemonic, genRegister) { (i, r) =>
          val expectedR = parseOperand(ParserUnderTest.registerOperand, r)
          parseAndCheckResult(s"$i $r", i, List(expectedR))
        }

      }

    }

    "manage instruction token separators" when {

      "space" in {
        parseAndCheckResult("MOV @R0, R1", "MOV", List(Operand.RegisterDeferredMode("R0"), Operand.RegisterMode("R1")))
      }

      "tab" in {
        parseAndCheckResult("MOV\t@R0,\tR1", "MOV", List(Operand.RegisterDeferredMode("R0"), Operand.RegisterMode("R1")))
      }

      "implied" in {
        parseAndCheckResult("MOV@R0,R1", "MOV", List(Operand.RegisterDeferredMode("R0"), Operand.RegisterMode("R1")))
      }

    }
  }
