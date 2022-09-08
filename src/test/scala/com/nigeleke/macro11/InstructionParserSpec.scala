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

import java.security.InvalidParameterException

class InstructionParserSpec extends AnyWordSpec with ScalaCheckDrivenPropertyChecks with Matchers:

  object ParserUnderTest extends InstructionParser
  import ParserUnderTest.*

  "The InstructionParser" should {

    given Shrink[String] = Shrink(_ => Stream.empty)
    import InstructionParserSpec.*

    def parseAndCheckResult(i: String, mnemonic: String, expectedParams: Seq[Operand]) =
      val expectedMnemonic = Mnemonic.valueOf(mnemonic)
      ParserUnderTest.parse(ParserUnderTest.instruction, i) match
        case Success(result, remainder) =>
          result should be(Instruction(expectedMnemonic, expectedParams))
          remainder.atEnd should be(true)
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
        import OperandParserSpec.*
        forAll(genDestinationOperandMnemonic, genAddressingModeOperand) { (i, ddd) =>
          val expectedDdd = parseOperand(ParserUnderTest.addressingModeOperand, ddd)
          parseAndCheckResult(s"$i $ddd", i, List(expectedDdd))
        }
      }

      "there are source and destination addressing operandParser operands" in {
        import OperandParserSpec.*
        forAll(genSourceDestinationOperandMnemonic, genAddressingModeOperand, genAddressingModeOperand) { (i, sss, ddd) =>
          val expectedSss = parseOperand(ParserUnderTest.addressingModeOperand, sss)
          val expectedDdd = parseOperand(ParserUnderTest.addressingModeOperand, ddd)
          parseAndCheckResult(s"$i $sss, $ddd", i, List(expectedSss, expectedDdd))
        }
      }

      "there is an address offset operandParser" in {
        import OperandParserSpec.*
        forAll(genAddressOffsetOperandMnemonic, genAddressOffsetOperand) { (i, a) =>
          val expectedA = parseOperand(ParserUnderTest.addressOffsetOperand, a)
          parseAndCheckResult(s"$i $a", i, List(expectedA))
        }
      }

      "there are register and address offset operands" in {
        import OperandParserSpec.*
        forAll(genRegisterAddressOffsetOperandMnemonic, genRegister, genAddressOffsetOperand) { (i, r, a) =>
          val expectedR = parseOperand(ParserUnderTest.registerOperand, r)
          val expectedA = parseOperand(ParserUnderTest.addressOffsetOperand, a)
          parseAndCheckResult(s"$i $r, $a", i, List(expectedR, expectedA))
        }
      }

      "there are register and destination addressing mode operands" in {
        import OperandParserSpec.*
        forAll(genRegisterDestinationOperandMnemonic, genRegister, genAddressingModeOperand) { (i, r, ddd) =>
          val expectedR   = parseOperand(ParserUnderTest.registerOperand, r)
          val expectedDdd = parseOperand(ParserUnderTest.addressingModeOperand, ddd)
          parseAndCheckResult(s"$i $r, $ddd", i, List(expectedR, expectedDdd))
        }
      }

      "there is a parameter count operand" in {
        import OperandParserSpec.*
        forAll(genParameterCountOperandMnemonic, genSymbol) { (i, p) =>
          val expectedP = parseOperand(ParserUnderTest.parameterCountOperand, p)
          parseAndCheckResult(s"$i $p", i, Seq(expectedP))
        }
      }

      "there is a trap parameter operand" in {
        import OperandParserSpec.*
        forAll(genTrapOperandMnemonic, genSymbol) { (i, tp) =>
          val expectedTp = parseOperand(ParserUnderTest.trapParameterOperand, tp)
          parseAndCheckResult(s"$i $tp", i, Seq(expectedTp))
        }
      }

      "there are register and source addressing mode operands" in {
        import OperandParserSpec.*
        forAll(genRegisterSourceOperandMnemonic, genRegister, genAddressingModeOperand) { (i, r, sss) =>
          val expectedR   = parseOperand(ParserUnderTest.registerOperand, r)
          val expectedSss = parseOperand(ParserUnderTest.addressingModeOperand, sss)
          parseAndCheckResult(s"$i $r, $sss", i, List(expectedR, expectedSss))
        }
      }

      "there is a register operand" in {
        import OperandParserSpec.*
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

object InstructionParserSpec:
  extension (m: Instruction.Mnemonic)
    def noOperands                    = m.fits()
    def destinationOperands           = m.fits(OperandRole.Destination)
    def sourceDestinationOperands     = m.fits(OperandRole.Source, OperandRole.Destination)
    def addressOffsetOperands         = m.fits(OperandRole.AddressOffsetSigned)
    def registerAddressOffsetOperands = m.fits(OperandRole.Register, OperandRole.AddressOffsetUnsigned)
    def registerDestinationOperands   = m.fits(OperandRole.Register, OperandRole.Destination)
    def parameterCountOperands        = m.fits(OperandRole.ParameterCount)
    def trapOperands                  = m.fits(OperandRole.Trap)
    def registerSourceOperands        = m.fits(OperandRole.Register, OperandRole.Source)
    def registerOperands              = m.fits(OperandRole.Register)

  private val mnemonics = Instruction.Mnemonic.values.toSeq

  val genNoOperandMnemonic                    = Gen.oneOf(mnemonics.filter(noOperands).map(_.toString))
  val genDestinationOperandMnemonic           = Gen.oneOf(mnemonics.filter(destinationOperands).map(_.toString))
  val genSourceDestinationOperandMnemonic     = Gen.oneOf(mnemonics.filter(sourceDestinationOperands).map(_.toString))
  val genAddressOffsetOperandMnemonic         = Gen.oneOf(mnemonics.filter(addressOffsetOperands).map(_.toString))
  val genRegisterAddressOffsetOperandMnemonic = Gen.oneOf(mnemonics.filter(registerAddressOffsetOperands).map(_.toString))
  val genRegisterDestinationOperandMnemonic   = Gen.oneOf(mnemonics.filter(registerDestinationOperands).map(_.toString))
  val genParameterCountOperandMnemonic        = Gen.oneOf(mnemonics.filter(parameterCountOperands).map(_.toString))
  val genTrapOperandMnemonic                  = Gen.oneOf(mnemonics.filter(trapOperands).map(_.toString))
  val genRegisterSourceOperandMnemonic        = Gen.oneOf(mnemonics.filter(registerSourceOperands).map(_.toString))
  val genRegisterOperandMnemonic              = Gen.oneOf(mnemonics.filter(registerOperands).map(_.toString))
