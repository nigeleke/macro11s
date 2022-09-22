package com.nigeleke.macro11

import com.nigeleke.macro11.ast.*
import com.nigeleke.macro11.ast.Instruction.*
import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalacheck.Test.Parameters.defaultVerbose.minSize

object Generators:
  // Utility...
  val genComment =
    for
      text    <- Gen.asciiPrintableStr
      comment <- Gen.oneOf("", s"; $text")
    yield comment

  val genSeparator = Gen.oneOf(" ", "\t", ",")

  val genSymbol =
    val symbolCharacters = Gen.oneOf(('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9') ++ Seq('$', '.'))
    for symbol <- Gen.nonEmptyListOf(symbolCharacters).suchThat(s => !('0' to '9').contains(s.head))
    yield s"${symbol.mkString}"
  val genSymbolOption = Gen.option(genSymbol)
  val genSymbolsList  = Gen.nonEmptyListOf(genSymbol)

  def genStringWithout(c: Char) = Gen.asciiPrintableStr.retryUntil(s => !s.contains(c))

  // Operands...
  val genRegister = Gen.oneOf((0 to 7).map(r => s"%$r") ++ (0 to 5).map(r => s"R$r") ++ Seq("SP", "PC"))

  val genBinaryOperator = Gen.oneOf("+", "-", "*", "/")
  val genUnaryOperator  = Gen.oneOf("+", "-")

  val genNumericTerm = Gen.numStr
  val genSymbolTerm  = Gen.oneOf(genSymbol, Gen.const("."))
  val genSingleQuoteTerm =
    for char <- Gen.asciiPrintableChar.suchThat(_ != ' ')
    yield s"'$char"
  val genDoubleQuoteTerm =
    for
      char1 <- Gen.asciiPrintableChar.suchThat(_ != ' ')
      char2 <- Gen.asciiPrintableChar.suchThat(_ != ' ')
    yield s"\"$char1$char2"
  val genUnaryOperatorTerm =
    for
      operator <- genUnaryOperator
      operand  <- Gen.oneOf(Gen.numStr, genSymbol).suchThat((_.nonEmpty))
    yield s"$operator$operand"
  val genTerm =
    Gen.oneOf(genNumericTerm, genSymbolTerm, genSingleQuoteTerm, genDoubleQuoteTerm, genUnaryOperatorTerm).suchThat(_.nonEmpty)

  val genBinaryOperatorExpression =
    for
      operator <- genBinaryOperator
      lhs      <- genTerm
      rhs      <- genTerm
    yield (s"$lhs$operator$rhs")
  val genSimpleExpression = genTerm
  val genExpression       = Gen.oneOf(genBinaryOperatorExpression, genSimpleExpression).suchThat(_.nonEmpty)
  val genExpressionTerm =
    for
      lt         <- Gen.const('<')
      gt         <- Gen.const('>')
      expression <- genExpression
    yield s"$lt$expression$gt"
  val genExpressionOption = Gen.option(genExpression)
  val genExpressionList   = Gen.nonEmptyListOf(genExpression)

  val genNumericExpression = genSymbol
  val genMacroArgument     = genExpressionTerm


  // format: off
  val genAddressingModeOperand =
    for // Source & Destinatioon Operands
      r <- genRegister
      s <- genSymbol
      operand <- Gen.oneOf(Seq(
        s"$r", s"@$r", s"($r)", s"($r)+", s"@($r)+", s"-($r)", s"@-($r)",
        s"$s($r)", s"@$s($r)", s"#$s", s"@#$s", s"$s", s"@$s"))
    yield operand
  // format: on

  val genAddressOffsetOperand = genSymbol

  // Instruction...
  private val mnemonics = Instruction.Mnemonic.values.toSeq

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

  // Statements...
  val genInstruction =
    for mnemonic <- genNoOperandMnemonic
    yield mnemonic.toString

  val genLabel =
    for
      label  <- genSymbol
      suffix <- Gen.oneOf(Seq(":", "::"))
    yield s"$label$suffix"
  val genLabelList = Gen.listOf(genLabel)

  // Directives...
  val genDecimalString =
    for s <- Gen.stringOfN(31, Gen.numChar)
    yield s

  val genDsablEnablArgument =
    Gen.oneOf("ABS", "AMA", "CDR", "CRF", "FPT", "LC", "LCM", "LSB", "MCL", "PNC", "REG", "GBL")

  val genFloat     = Gen.double.map(BigDecimal(_).toString)
  val genFloatList = Gen.nonEmptyListOf(genFloat)

  val genPSectArguments =
    val validArgs =
      List(Seq("RO", "RW"), Seq("I", "D"), Seq("GBL", "LCL"), Seq("ABS", "REL"), Seq("CON", "OVR"), Seq("SAV", "NOSAV"))
    val argumentChoice = Gen.oneOf(validArgs)
    val argument       = argumentChoice.flatMap(Gen.oneOf(_))
    Gen.nonEmptyListOf(argument)

  private val stringDelimiter = Gen.asciiPrintableChar.retryUntil(_ != ' ')

  val genRad50String =
    val validRad50Characters = Gen.oneOf(('A' to 'Z') ++ ('0' to '9') ++ "$. ".toSeq)
    Gen.stringOf(validRad50Characters)

  val genRad50Symbol =
    val validRad50Characters = Gen.oneOf(('A' to 'Z') ++ ('0' to '9') ++ "$.".toSeq)
    Gen.nonEmptyListOf(validRad50Characters).map(_.mkString)

  val genRad50DelimitedString =
    for
      d <- stringDelimiter
      s <- genRad50String.retryUntil(s => !s.contains(d))
    yield s"$d$s$d"

  val genMaybeRadix = Gen.option(Gen.oneOf("2", "8", "10"))

  val genSimpleDelimitedString =
    for
      d <- stringDelimiter
      s <- genStringWithout(d)
    yield s"$d$s$d"

  // Program...
  val genStatement =
    for
      labels      <- genLabelList.map(_.mkString(""))
      instruction <- genInstruction
      comment     <- genComment
    yield s"$labels$instruction$comment"

  val genDirective = Gen.const(".TITLE Macro11Parser")
