package com.nigeleke.macro11

import com.nigeleke.macro11.ast.*
import com.nigeleke.macro11.parser.UtilityParser

import org.scalacheck.Gen
import org.scalacheck.Shrink
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class UtilityParserSpec extends AnyWordSpec with ScalaCheckDrivenPropertyChecks with Matchers:

  object ParserUnderTest extends UtilityParser {
    def optionalSymbolComment = opt(symbol) ~ opt(comment) ^^ { case s ~ c => (s, c) }
  }
  import ParserUnderTest.*

  "The UtilityParser" should {

    given Shrink[_] = Shrink(_ => Stream.empty)
    import Generators.*

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
      forAll(genMaybeSymbol, genMaybeComment) { (maybeS, maybeC) =>
        val s = maybeS.getOrElse("")
        val c = maybeC.getOrElse("")
        ParserUnderTest.parse(ParserUnderTest.optionalSymbolComment, s"$s $c") match
          case Success(result, _)  => result should be((maybeS, maybeC.map(Comment.apply)))
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }
    }

  }
