package com.nigeleke.macro11

import com.nigeleke.macro11.ast.*
import com.nigeleke.macro11.ast.Instruction.*
import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalacheck.Test.Parameters.defaultVerbose.minSize

object Generators:
  // Utility...
  val genComment: Gen[String] =
    for
      text    <- Gen.asciiPrintableStr
      comment <- Gen.oneOf("", s"; $text")
    yield comment

  val genSymbol: Gen[String] =
    val symbolCharacters = Gen.oneOf(('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9') ++ Seq('$', '.'))
    for symbol <- Gen.nonEmptyListOf(symbolCharacters).suchThat(s => !('0' to '9').contains(s.head))
    yield s"${symbol.mkString}"
  val genSymbolOption: Gen[Option[String]] = Gen.option(genSymbol)
  val genSymbolsList: Gen[List[String]]    = Gen.nonEmptyListOf(genSymbol)

  def genStringWithout(c: Char): Gen[String] = Gen.asciiPrintableStr.retryUntil(s => !s.contains(c))

  // Operands...
  val genRegister: Gen[String] = Gen.oneOf((0 to 7).map(r => s"%$r") ++ (0 to 5).map(r => s"R$r") ++ Seq("SP", "PC"))

  val genBinaryOperator: Gen[String] = Gen.oneOf("+", "-", "*", "/")
  val genUnaryOperator: Gen[String]  = Gen.oneOf("+", "-")

  val genNumericTerm: Gen[String] = Gen.numStr
  val genSymbolTerm: Gen[String]  = Gen.oneOf(genSymbol, Gen.const("."))
  val genSingleQuoteTerm: Gen[String] =
    for char <- Gen.asciiPrintableChar.suchThat(_ != ' ')
    yield s"'$char"
  val genDoubleQuoteTerm: Gen[String] =
    for
      char1 <- Gen.asciiPrintableChar.suchThat(_ != ' ')
      char2 <- Gen.asciiPrintableChar.suchThat(_ != ' ')
    yield s"\"$char1$char2"
  val genUnaryOperatorTerm: Gen[String] =
    for
      operator <- genUnaryOperator
      operand  <- Gen.oneOf(Gen.numStr, genSymbol).suchThat(_.nonEmpty)
    yield s"$operator$operand"
  val genTerm: Gen[String] =
    Gen.oneOf(genNumericTerm, genSymbolTerm, genSingleQuoteTerm, genDoubleQuoteTerm, genUnaryOperatorTerm).suchThat(_.nonEmpty)

  val genBinaryOperatorExpression: Gen[String] =
    for
      operator <- genBinaryOperator
      lhs      <- genTerm
      rhs      <- genTerm
    yield s"$lhs$operator$rhs"
  val genSimpleExpression: Gen[String] = genTerm
  val genExpression: Gen[String]       = Gen.oneOf(genBinaryOperatorExpression, genSimpleExpression).suchThat(_.nonEmpty)
  val genExpressionTerm: Gen[String] =
    for
      lt         <- Gen.const('<')
      gt         <- Gen.const('>')
      expression <- genExpression
    yield s"$lt$expression$gt"
  val genExpressionOption: Gen[Option[String]] = Gen.option(genExpression)
  val genExpressionList: Gen[List[String]]     = Gen.nonEmptyListOf(genExpression)

  val genRegisterExpression: Gen[String] = genRegister
  val genMacroArgument: Gen[String]      = genExpressionTerm


  // format: off
  val genAddressingModeOperand: Gen[String] =
    for // Source & Destinatioon Operands
      r <- genRegister
      s <- genSymbol
      operand <- Gen.oneOf(Seq(
        s"$r", s"@$r", s"($r)", s"($r)+", s"@($r)+", s"-($r)", s"@-($r)",
        s"$s($r)", s"@$s($r)", s"#$s", s"@#$s", s"$s", s"@$s"))
    yield operand
  // format: on

  val genAddressOffsetOperand: Gen[String] = genExpression

  // Instruction...
  private val mnemonics = Instruction.Mnemonic.values.toSeq

  extension (m: Instruction.Mnemonic)
    def noOperands                    = m.requiring()
    def destinationOperands           = m.requiring(OperandRole.Destination)
    def sourceDestinationOperands     = m.requiring(OperandRole.Source, OperandRole.Destination)
    def addressOffsetOperands         = m.requiring(OperandRole.AddressOffsetSigned)
    def registerAddressOffsetOperands = m.requiring(OperandRole.Register, OperandRole.AddressOffsetUnsigned)
    def registerDestinationOperands   = m.requiring(OperandRole.Register, OperandRole.Destination)
    def parameterCountOperands        = m.requiring(OperandRole.ParameterCount)
    def trapOperands                  = m.requiring(OperandRole.Trap)
    def registerSourceOperands        = m.requiring(OperandRole.Register, OperandRole.Source)
    def registerOperands              = m.requiring(OperandRole.Register)

  val genNoOperandMnemonic: Gen[String]                = Gen.oneOf(mnemonics.filter(noOperands).map(_.toString))
  val genDestinationOperandMnemonic: Gen[String]       = Gen.oneOf(mnemonics.filter(destinationOperands).map(_.toString))
  val genSourceDestinationOperandMnemonic: Gen[String] = Gen.oneOf(mnemonics.filter(sourceDestinationOperands).map(_.toString))
  val genAddressOffsetOperandMnemonic: Gen[String]     = Gen.oneOf(mnemonics.filter(addressOffsetOperands).map(_.toString))
  val genRegisterAddressOffsetOperandMnemonic: Gen[String] =
    Gen.oneOf(mnemonics.filter(registerAddressOffsetOperands).map(_.toString))
  val genRegisterDestinationOperandMnemonic: Gen[String] = Gen.oneOf(mnemonics.filter(registerDestinationOperands).map(_.toString))
  val genParameterCountOperandMnemonic: Gen[String]      = Gen.oneOf(mnemonics.filter(parameterCountOperands).map(_.toString))
  val genTrapOperandMnemonic: Gen[String]                = Gen.oneOf(mnemonics.filter(trapOperands).map(_.toString))
  val genRegisterSourceOperandMnemonic: Gen[String]      = Gen.oneOf(mnemonics.filter(registerSourceOperands).map(_.toString))
  val genRegisterOperandMnemonic: Gen[String]            = Gen.oneOf(mnemonics.filter(registerOperands).map(_.toString))

  // Statements...
  val genInstruction: Gen[String] =
    for mnemonic <- genNoOperandMnemonic
    yield mnemonic

  val genLabel: Gen[String] =
    for
      label  <- genSymbol
      suffix <- Gen.oneOf(Seq(":", "::"))
    yield s"$label$suffix"
  val genLabelList: Gen[List[String]] = Gen.listOf(genLabel)

  // Directives...
  val genDecimalString: Gen[String] =
    for s <- Gen.stringOfN(31, Gen.numChar)
    yield s

  val genDsablEnablArgument: Gen[String] =
    val validArgs = List("ABS", "AMA", "CDR", "CRF", "FPT", "LC", "LCM", "LSB", "MCL", "PNC", "REG", "GBL")
    Gen.oneOf(validArgs)

  val genFloat: Gen[String]           = Gen.double.map(BigDecimal(_).toString)
  val genFloatList: Gen[List[String]] = Gen.nonEmptyListOf(genFloat)

  val genPSectArguments: Gen[List[String]] =
    val validArgs =
      List(Seq("RO", "RW"), Seq("I", "D"), Seq("GBL", "LCL"), Seq("ABS", "REL"), Seq("CON", "OVR"), Seq("SAV", "NOSAV"))
    val argumentChoice = Gen.oneOf(validArgs)
    val argument       = argumentChoice.flatMap(Gen.oneOf)
    Gen.nonEmptyListOf(argument)

  private val stringDelimiter = Gen.asciiPrintableChar.retryUntil(_ != ' ')

  val genRad50String: Gen[String] =
    val validRad50Characters = Gen.oneOf(('A' to 'Z') ++ ('0' to '9') ++ "$. ".toSeq)
    Gen.stringOf(validRad50Characters)

  val genRad50Symbol: Gen[String] =
    val validRad50Characters = Gen.oneOf(('A' to 'Z') ++ ('0' to '9') ++ "$.".toSeq)
    Gen.nonEmptyListOf(validRad50Characters).map(_.mkString)

  val genRad50DelimitedString: Gen[String] =
    for
      d <- stringDelimiter
      s <- genRad50String.retryUntil(s => !s.contains(d))
    yield s"$d$s$d"

  val genMaybeRadix: Gen[Option[String]] = Gen.option(Gen.oneOf("2", "8", "10"))

  val genSimpleDelimitedString: Gen[String] =
    for
      d <- stringDelimiter
      s <- genStringWithout(d)
    yield s"$d$s$d"

  // Program...
  val genStatement: Gen[String] =
    for
      labels      <- genLabelList.map(_.mkString(""))
      instruction <- genInstruction
      comment     <- genComment
    yield s"$labels$instruction$comment"

  val genDirective: Gen[String] = Gen.const(".TITLE Macro11Parser")
