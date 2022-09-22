package com.nigeleke.macro11

import com.nigeleke.macro11.ast.*
import com.nigeleke.macro11.parser.*
import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalatest.*
import org.scalatest.matchers.should.*
import org.scalatest.wordspec.*

class Macro11ParserSpec extends AnyWordSpec with Matchers:

  object ParserUnderTest extends Macro11Parser with UtilityParser
  import ParserUnderTest.*

  "A Macro11Parser" should {

    import com.nigeleke.macro11.Generators.*

    "parse from a string stream" in {
      ParserUnderTest.parse(ParserUnderTest.program, "\n") match
        case Success(result, _)  => result should be(a[Program])
        case Failure(message, _) => fail(message)
        case Error(error, _)     => fail(error)
    }

    "parse a statement line" in {
      forAll(genStatement) { statement =>
        val expectedStatement = ParserUnderTest.parse(ParserUnderTest.statement, s"$statement\n").get
        val expectedProgram   = Program(List(expectedStatement))
        ParserUnderTest.parse(ParserUnderTest.program, statement) match
          case Success(result, _) =>
            result should be(expectedProgram)
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }
    }

    "parse a directive line" in {
      forAll(genDirective) { directive =>
        val expectedDirective = ParserUnderTest.parse(ParserUnderTest.directive, s"$directive\n").get
        val expectedProgram   = Program(List(expectedDirective))
        ParserUnderTest.parse(ParserUnderTest.program, directive) match
          case Success(result, _) =>
            result should be(expectedProgram)
          case Failure(message, _) => fail(message)
          case Error(error, _)     => fail(error)
      }
    }

  }
