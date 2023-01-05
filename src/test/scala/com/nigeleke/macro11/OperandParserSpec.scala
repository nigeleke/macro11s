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

    def parseAndCheckAddressingModeResult(mode: String, expectedMode: AddressingMode) = {
      val expectedOperand = Operand.AddressingModeOperand(expectedMode)
      parseAndCheckResult(ParserUnderTest.addressingModeOperand, mode, expectedOperand)
    }

    "parse addressingMode operandRoles" when {

      "register operand - r" in {
        forAll(genRegister) { r =>
          val expectedRegister = ParserUnderTest.parse(ParserUnderTest.expression, r).get
          val expectedMode     = AddressingMode.Register(expectedRegister)
          parseAndCheckAddressingModeResult(r, expectedMode)
        }
      }

      "register deferred operand - @r" in {
        forAll(genRegister) { r =>
          val expectedRegister = ParserUnderTest.parse(ParserUnderTest.expression, r).get
          val expectedMode     = AddressingMode.RegisterDeferred(expectedRegister)
          parseAndCheckAddressingModeResult(s"@$r", expectedMode)
        }
      }

      "register deferred operand - (r)" in {
        forAll(genRegister) { r =>
          val expectedRegister = ParserUnderTest.parse(ParserUnderTest.expression, r).get
          val expectedMode     = AddressingMode.RegisterDeferred(expectedRegister)
          parseAndCheckAddressingModeResult(s"($r)", expectedMode)
        }
      }

      "auto increment operand - (r)+" in {
        forAll(genRegister) { r =>
          val expectedRegister = ParserUnderTest.parse(ParserUnderTest.expression, r).get
          val expectedMode     = AddressingMode.AutoIncrement(expectedRegister)
          parseAndCheckAddressingModeResult(s"($r)+", expectedMode)
        }
      }

      "auto increment deferred operand - @(r)+" in {
        forAll(genRegister) { r =>
          val expectedRegister = ParserUnderTest.parse(ParserUnderTest.expression, r).get
          val expectedMode     = AddressingMode.AutoIncrementDeferred(expectedRegister)
          parseAndCheckAddressingModeResult(s"@($r)+", expectedMode)
        }
      }

      "auto decrement operand - -(r)" in {
        forAll(genRegister) { r =>
          val expectedRegister = ParserUnderTest.parse(ParserUnderTest.expression, r).get
          val expectedMode     = AddressingMode.AutoDecrement(expectedRegister)
          parseAndCheckAddressingModeResult(s"-($r)", expectedMode)
        }
      }

      "auto decrement deferred operand - @-(r)" in {
        forAll(genRegister) { r =>
          val expectedRegister = ParserUnderTest.parse(ParserUnderTest.expression, r).get
          val expectedMode     = AddressingMode.AutoDecrementDeferred(expectedRegister)
          parseAndCheckAddressingModeResult(s"@-($r)", expectedMode)
        }
      }

      "index operand - symbol(r)" in {
        forAll(genSymbol, genRegister) { (s, r) =>
          val expectedExpression = ParserUnderTest.parse(ParserUnderTest.expression, s).get
          val expectedRegister   = ParserUnderTest.parse(ParserUnderTest.expression, r).get
          val expectedMode       = AddressingMode.Index(expectedExpression, expectedRegister)
          parseAndCheckAddressingModeResult(s"$s($r)", expectedMode)
        }
      }

      "index deferred operand - @symbol(r)" in {
        forAll(genSymbol, genRegister) { (s, r) =>
          val expectedExpression = ParserUnderTest.parse(ParserUnderTest.expression, s).get
          val expectedRegister   = ParserUnderTest.parse(ParserUnderTest.expression, r).get
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
          println(s"Symbol $s")
          val expectedExpression = ParserUnderTest.parse(ParserUnderTest.expression, s).get
          println(s"ExpectedExpression $expectedExpression")
          val expectedMode = AddressingMode.Relative(expectedExpression)
          println(s"ExpectedMode $expectedMode")
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
      forAll(genAddressOffsetOperand) { operand =>
        val expectedExpression = ParserUnderTest.parse(ParserUnderTest.expression, operand).get
        val expectedOperand    = Operand.AddressOffsetOperand(expectedExpression)
        parseAndCheckResult(ParserUnderTest.addressOffsetOperand, operand, expectedOperand)
      }
    }

    "parse register operandRoles" in {
      forAll(genRegisterExpression) { register =>
        val expectedExpression = ParserUnderTest.parse(ParserUnderTest.expression, register).get
        val expectedOperand    = Operand.RegisterOperand(expectedExpression)
        parseAndCheckResult(ParserUnderTest.registerOperand, register, expectedOperand)
      }
    }

    "parse parameterCount operandRoles" in {
      forAll(genExpression) { count =>
        val expectedExpression = ParserUnderTest.parse(ParserUnderTest.expression, count).get
        val expectedOperand    = Operand.ParameterCountOperand(expectedExpression)
        parseAndCheckResult(ParserUnderTest.parameterCountOperand, count, expectedOperand)
      }
    }

    "parse trapParameter operandRoles" in {
      forAll(genExpression) { id =>
        val expectedExpression = ParserUnderTest.parse(ParserUnderTest.expression, id).get
        val expectedOperand    = Operand.TrapParameterOperand(expectedExpression)
        parseAndCheckResult(ParserUnderTest.trapParameterOperand, id, expectedOperand)
      }
    }

  }
