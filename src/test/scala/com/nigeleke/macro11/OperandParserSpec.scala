package com.nigeleke.macro11

import com.nigeleke.macro11.ast.*
import com.nigeleke.macro11.parser.*
import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.*
import org.scalatestplus.scalacheck.*

class OperandParserSpec extends AnyWordSpec with ScalaCheckPropertyChecks with Matchers:

  object ParserUnderTest extends OperandParser
  import ParserUnderTest.*

  "The OperandParser" should {

    import com.nigeleke.macro11.Generators.*

    def parseAndCheckResult(parser: Parser[Operand], p: String, expectedOperand: Operand) =
      ParserUnderTest.parse(parser, p) match
        case Success(result, _)  => result should be(expectedOperand)
        case Failure(message, _) => fail(message)
        case Error(error, _)     => fail(error)

    def parseAndCheckAddressingModeResult(am: String, expectedMode: AddressingMode) =
      parseAndCheckResult(ParserUnderTest.addressingModeOperand, am, Operand.AddressingModeOperand(expectedMode))

    "parse addressingMode operandRoles" when {

      "register operand - r" in {
        forAll(genRegister) { r =>
          val expectedMode = AddressingMode.Register(RegisterExpression(RegisterTerm(r)))
          parseAndCheckAddressingModeResult(r, expectedMode)
        }
      }

      "register deferred operand - @r" in {
        forAll(genRegister) { r =>
          val expectedMode = AddressingMode.RegisterDeferred(RegisterExpression(RegisterTerm(r)))
          parseAndCheckAddressingModeResult(s"@$r", expectedMode)
        }
      }

      "register deferred operand - (r)" in {
        forAll(genRegister) { r =>
          val expectedMode = AddressingMode.RegisterDeferred(RegisterExpression(RegisterTerm(r)))
          parseAndCheckAddressingModeResult(s"($r)", expectedMode)
        }
      }

      "auto increment operand - (r)+" in {
        forAll(genRegister) { r =>
          val expectedMode = AddressingMode.AutoIncrement(RegisterExpression(RegisterTerm(r)))
          parseAndCheckAddressingModeResult(s"($r)+", expectedMode)
        }
      }

      "auto increment deferred operand - @(r)+" in {
        forAll(genRegister) { r =>
          val expectedMode = AddressingMode.AutoIncrementDeferred(RegisterExpression(RegisterTerm(r)))
          parseAndCheckAddressingModeResult(s"@($r)+", expectedMode)
        }
      }

      "auto decrement operand - -(r)" in {
        forAll(genRegister) { r =>
          val expectedMode = AddressingMode.AutoDecrement(RegisterExpression(RegisterTerm(r)))
          parseAndCheckAddressingModeResult(s"-($r)", expectedMode)
        }
      }

      "auto decrement deferred operand - @-(r)" in {
        forAll(genRegister) { r =>
          val expectedMode = AddressingMode.AutoDecrementDeferred(RegisterExpression(RegisterTerm(r)))
          parseAndCheckAddressingModeResult(s"@-($r)", expectedMode)
        }
      }

      "index operand - symbol(r)" in {
        forAll(genSymbol, genRegister) { (s, r) =>
          val expectedExpression = ParserUnderTest.parse(ParserUnderTest.expression, s).get
          val expectedRegister   = ParserUnderTest.parse(ParserUnderTest.registerExpression, r).get
          val expectedMode       = AddressingMode.Index(expectedExpression, expectedRegister)
          parseAndCheckAddressingModeResult(s"$s($r)", expectedMode)
        }
      }

      "index deferred operand - @symbol(r)" in {
        forAll(genSymbol, genRegister) { (s, r) =>
          val expectedExpression = ParserUnderTest.parse(ParserUnderTest.expression, s).get
          val expectedRegister   = ParserUnderTest.parse(ParserUnderTest.registerExpression, r).get
          val expectedMode       = AddressingMode.IndexDeferred(expectedExpression, expectedRegister)
          parseAndCheckAddressingModeResult(s"@$s($r)", expectedMode)
        }
      }

      "immediate operand - #expression" in {
        forAll(genSymbol) { s =>
          val expectedExpression = ParserUnderTest.parse(ParserUnderTest.expression, s).get
          val expectedMode       = AddressingMode.Immediate(expectedExpression)
          parseAndCheckAddressingModeResult(s"#$s", expectedMode)
        }
      }

      "absolute operand - @#expression" in {
        forAll(genSymbol) { s =>
          val expectedExpression = ParserUnderTest.parse(ParserUnderTest.expression, s).get
          val expectedMode       = AddressingMode.Absolute(expectedExpression)
          parseAndCheckAddressingModeResult(s"@#$s", expectedMode)
        }
      }

      "relative operand - expression" in {
        forAll(genSymbol) { s =>
          val expectedExpression = ParserUnderTest.parse(ParserUnderTest.expression, s).get
          val expectedMode       = AddressingMode.Relative(expectedExpression)
          parseAndCheckAddressingModeResult(s, expectedMode)
        }
      }

      "relative deferred operand - @expression" in {
        forAll(genSymbol) { s =>
          val expectedExpression = ParserUnderTest.parse(ParserUnderTest.expression, s).get
          val expectedMode       = AddressingMode.RelativeDeferred(expectedExpression)
          parseAndCheckAddressingModeResult(s"@$s", expectedMode)
        }
      }
    }

    "parse addressOffset operandRoles" in {
      forAll(genAddressOffsetOperand) { a =>
        parseAndCheckResult(ParserUnderTest.addressOffsetOperand, a, Operand.AddressOffsetOperand(a))
      }
    }

    "parse register operandRoles" in {
      forAll(genRegister) { r =>
        parseAndCheckResult(ParserUnderTest.registerOperand, r, Operand.RegisterOperand(r))
      }
    }

    "parse parameterCount operandRoles" in {
      forAll(genSymbol) { s =>
        parseAndCheckResult(ParserUnderTest.parameterCountOperand, s, Operand.ParameterCountOperand(s))
      }
    }

    "parse trapParameter operandRoles" in {
      forAll(genSymbol) { s =>
        parseAndCheckResult(ParserUnderTest.trapParameterOperand, s, Operand.TrapParameterOperand(s))
      }
    }

  }
