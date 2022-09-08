package com.nigeleke.macro11

import com.nigeleke.macro11.ast.*
import com.nigeleke.macro11.parser.*
import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.*
import org.scalatestplus.scalacheck.*

class AddressingModeOperandParserSpec extends AnyWordSpec with ScalaCheckDrivenPropertyChecks with Matchers:

  object ParserUnderTest extends OperandParser
  import ParserUnderTest.*

  "The OperandParser" should {

    given Shrink[String] = Shrink(_ => Stream.empty)
    import OperandParserSpec.*

    def parseAndCheckResult(parser: Parser[Operand], p: String, expectedOperand: Operand) =
      ParserUnderTest.parse(parser, p) match
        case Success(result, remainder) =>
          result should be(expectedOperand)
          remainder.atEnd should be(true)
        case Failure(message, _) => fail(message)
        case Error(error, _)     => fail(error)

    "parse addressingMode operands" when {

      "register operand - r" in {
        forAll(genRegister) { r =>
          parseAndCheckResult(ParserUnderTest.addressingModeOperand, r, Operand.RegisterMode(r))
        }
      }

      "register deferred operand - @r" in {
        forAll(genRegister) { r =>
          parseAndCheckResult(ParserUnderTest.addressingModeOperand, s"@$r", Operand.RegisterDeferredMode(r))
        }
      }

      "register deferred operand - (r)" in {
        forAll(genRegister) { r =>
          parseAndCheckResult(ParserUnderTest.addressingModeOperand, s"($r)", Operand.RegisterDeferredMode(r))
        }
      }

      "auto increment operand - (r)+" in {
        forAll(genRegister) { r =>
          parseAndCheckResult(ParserUnderTest.addressingModeOperand, s"($r)+", Operand.AutoIncrementMode(r))
        }
      }

      "auto increment deferred operand - @(r)+" in {
        forAll(genRegister) { r =>
          parseAndCheckResult(ParserUnderTest.addressingModeOperand, s"@($r)+", Operand.AutoIncrementDeferredMode(r))
        }
      }

      "auto decrement operand - -(r)" in {
        forAll(genRegister) { r =>
          parseAndCheckResult(ParserUnderTest.addressingModeOperand, s"-($r)", Operand.AutoDecrementMode(r))
        }
      }

      "auto decrement deferred operand - @-(r)" in {
        forAll(genRegister) { r =>
          parseAndCheckResult(ParserUnderTest.addressingModeOperand, s"@-($r)", Operand.AutoDecrementDeferredMode(r))
        }
      }

      "index operand - symbol(r)" in {
        forAll(genSymbol, genRegister) { (s, r) =>
          parseAndCheckResult(ParserUnderTest.addressingModeOperand, s"$s($r)", Operand.IndexMode(s, r))
        }
      }

      "index deferred operand - @symbol(r)" in {
        forAll(genSymbol, genRegister) { (s, r) =>
          parseAndCheckResult(ParserUnderTest.addressingModeOperand, s"@$s($r)", Operand.IndexDeferredMode(s, r))
        }
      }

      "immediate operand - #expression" in {
        forAll(genSymbol) { s =>
          parseAndCheckResult(ParserUnderTest.addressingModeOperand, s"#$s", Operand.ImmediateMode(s))
        }
      }

      "absolute operand - @#expression" in {
        forAll(genSymbol) { s =>
          parseAndCheckResult(ParserUnderTest.addressingModeOperand, s"@#$s", Operand.AbsoluteMode(s))
        }
      }

      "relative operand - expression" in {
        forAll(genSymbol) { s =>
          parseAndCheckResult(ParserUnderTest.addressingModeOperand, s, Operand.RelativeMode(s))
        }
      }

      "relative deferred operand - @expression" in {
        forAll(genSymbol) { s =>
          parseAndCheckResult(ParserUnderTest.addressingModeOperand, s"@$s", Operand.RelativeDeferredMode(s))
        }
      }
    }

    "parse addressOffset operands" in {
      forAll(genAddressOffsetOperand) { a =>
        parseAndCheckResult(ParserUnderTest.addressOffsetOperand, a, Operand.AddressOffsetOperand(a))
      }
    }

    "parse register operands" in {
      forAll(genRegister) { r =>
        parseAndCheckResult(ParserUnderTest.registerOperand, r, Operand.RegisterOperand(r))
      }
    }

    "parse parameterCount operands" in {
      forAll(genSymbol) { s =>
        parseAndCheckResult(ParserUnderTest.parameterCountOperand, s, Operand.ParameterCountOperand(s))
      }
    }

    "parse trapParameter operands" in {
      forAll(genSymbol) { s =>
        parseAndCheckResult(ParserUnderTest.trapParameterOperand, s, Operand.TrapParameterOperand(s))
      }
    }

  }

object OperandParserSpec:
  val genRegister = Gen.oneOf((0 to 7).map(r => s"%$r") ++ (0 to 5).map(r => s"R$r") ++ Seq("SP", "PC"))
  val genSymbol =
    for symbol <- Gen
        .someOf(('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9') ++ "$.".toSeq)
        .suchThat(_.nonEmpty)
    yield String(symbol.toArray)

  // format: off
  val genAddressingModeOperand = for // Source & Destinatioon Operands
    r <- genRegister
    s <- genSymbol
    operand <- Gen.oneOf(Seq(
      s"$r", s"@$r", s"($r)", s"($r)+", s"@($r)+", s"-($r)", s"@-($r)",
      s"$s($r)", s"@$s($r)", s"#$s", s"@#$s", s"$s", s"@$s"))
  yield operand
  // format: on

  val genAddressOffsetOperand = genSymbol
