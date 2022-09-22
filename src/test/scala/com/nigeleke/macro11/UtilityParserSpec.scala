package com.nigeleke.macro11

import com.nigeleke.macro11.ast.*
import com.nigeleke.macro11.parser.UtilityParser
import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.util.function.BinaryOperator

class UtilityParserSpec extends AnyWordSpec with Matchers:

  object ParserUnderTest extends UtilityParser:
    def optionalSymbolComment: Parser[(Option[String], Comment)] = opt(symbol) ~ comment ^^ { case s ~ c => (s, c) }

  import ParserUnderTest.*

  "The UtilityParser" should {

    import com.nigeleke.macro11.Generators.*

    "parse symbol" when {
      "only symbol" in {
        forAll(genSymbol) { s =>
          ParserUnderTest.parse(ParserUnderTest.symbol, s) match
            case Success(result, _)  => result should be(s)
            case Failure(message, _) => fail(message)
            case Error(error, _)     => fail(error)
        }
      }

      "trailing non-symbol text" in {
        forAll(genSymbol) { s =>
          ParserUnderTest.parse(ParserUnderTest.symbol, s"$s more") match
            case Success(result, _)  => result should be(s)
            case Failure(message, _) => fail(message)
            case Error(error, _)     => fail(error)
        }
      }
    }

    "parse comment" in {
      forAll(genComment) { s =>
        ParserUnderTest.parse(ParserUnderTest.comment, s) match
          case Success(result, _)  => result should be(Comment(s))
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }
    }

    "parse opt(symbol) followed by opt(comment)" in {
      forAll(genSymbolOption, genComment) { (maybeS, comment) =>
        val s = maybeS.getOrElse("")
        ParserUnderTest.parse(ParserUnderTest.optionalSymbolComment, s"$s $comment") match
          case Success(result, _)  => result should be((maybeS, Comment(comment)))
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }
    }

    "parse a term" when {
      "numeric" in {
        forAll(Gen.numStr.suchThat(_.nonEmpty)) { digits =>
          ParserUnderTest.parse(ParserUnderTest.term, digits) match
            case Success(result, _)  => result should be(NumericTerm(digits))
            case Failure(message, _) => fail(message)
            case Error(error, _)     => fail(error)
        }
      }

      "a symbol" in {
        forAll(genSymbol) { symbol =>
          ParserUnderTest.parse(ParserUnderTest.term, symbol) match
            case Success(result, _)  => result should be(SymbolTerm(symbol))
            case Failure(message, _) => fail(message)
            case Error(error, _)     => fail(error)
        }
      }

      "the current location counter ." in {
        ParserUnderTest.parse(ParserUnderTest.term, ".") match
          case Success(result, _)  => result should be(CurrentLocationCounterTerm)
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }

      "a single quote followed by one ASCII character" in {
        forAll(Gen.asciiPrintableChar.suchThat(_ != ' ')) { char =>
          ParserUnderTest.parse(ParserUnderTest.term, s"'$char") match
            case Success(result, _)  => result should be(AsciiChar1Term(s"$char"))
            case Failure(message, _) => fail(message)
            case Error(error, _)     => fail(error)
        }
      }

      "a double quote followed by two ASCII characters" in {
        forAll(Gen.asciiPrintableChar.suchThat(_ != ' '), Gen.asciiPrintableChar.suchThat(_ != ' ')) { (char1, char2) =>
          ParserUnderTest.parse(ParserUnderTest.term, s"\"$char1$char2") match
            case Success(result, _)  => result should be(AsciiChar2Term(s"$char1$char2"))
            case Failure(message, _) => fail(message)
            case Error(error, _)     => fail(error)
        }
      }

      "an expression surrounded by angle brackets" in {
        forAll(genExpressionTerm) { term =>
          ParserUnderTest.parse(ParserUnderTest.term, term) match
            case Success(result, _)  => result should be(a[ExpressionTerm])
            case Failure(message, _) => fail(message)
            case Error(error, _)     => fail(error)
        }
      }

      "a unary operator followed by a number or a symbol" in {
        forAll(genUnaryOperatorTerm) { term =>
          ParserUnderTest.parse(ParserUnderTest.term, term) match
            case Success(result, _)  => result should be(UnaryOperatorTerm(term.head.toString, term.tail))
            case Failure(message, _) => fail(message)
            case Error(error, _)     => fail(error)
        }
      }
    }

    "parse an expression" when {
      "a binary operator" in {
        forAll(genBinaryOperatorExpression) { expression =>
          ParserUnderTest.parse(ParserUnderTest.expression, expression) match
            case Success(result, _)  => result should be(a[BinaryOperatorExpression])
            case Failure(message, _) => fail(message)
            case Error(error, _)     => fail(error)

        }
      }

      "a single term" in {
        forAll(genTerm) { term =>
          ParserUnderTest.parse(ParserUnderTest.expression, term) match
            case Success(result, _)  => result should be(a[SimpleExpression])
            case Failure(message, _) => fail(message)
            case Error(error, _)     => fail(error)
        }
      }

    }

  }
