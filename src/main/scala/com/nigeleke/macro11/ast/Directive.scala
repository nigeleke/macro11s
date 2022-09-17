package com.nigeleke.macro11.ast

trait Directive

// TODO: Amend String to StringExpression
case class AsciiDirective(param: DelimitedString, comment: Comment) extends Directive

case class AscizDirective(param: DelimitedString, comment: Comment) extends Directive

case class ASectDirective(comment: Comment) extends Directive

// TODO: Amend String to NumericExpression
case class BlkbDirective(expression: String, comment: Comment) extends Directive

case class BlkwDirective(expression: String, comment: Comment) extends Directive

case class ByteDirective(expressions: List[String], comment: Comment) extends Directive

case class CrossDirective(symbols: List[String], comment: Comment) extends Directive

case class CSectDirective(name: String, comment: Comment) extends Directive

case class DsablDirective(argument: EnablDsablArgument, comment: Comment) extends Directive

case class EnablDirective(argument: EnablDsablArgument, comment: Comment) extends Directive

case class EndDirective(expression: String, comment: Comment) extends Directive

case class EndcDirective(comment: Comment) extends Directive

case class EvenDirective(comment: Comment) extends Directive

case class Flt2Directive(floats: List[BigDecimal], comment: Comment) extends Directive

case class Flt4Directive(floats: List[BigDecimal], comment: Comment) extends Directive

case class GlobalDirective(globals: List[String], comment: Comment) extends Directive

case class IdentDirective(content: DelimitedString, comment: Comment) extends Directive

case class IfDirective(condition: String, arguments: List[String], comment: Comment) extends Directive

case class IffDirective(comment: Comment) extends Directive

case class IftDirective(comment: Comment) extends Directive

case class IftfDirective(comment: Comment) extends Directive

case class IifDirective(condition: String, arguments: List[String], instruction: Instruction, comment: Comment) extends Directive

case class IncludeDirective(file: DelimitedString, comment: Comment) extends Directive

case class LibraryDirective(file: DelimitedString, comment: Comment) extends Directive

case class LimitDirective(comment: Comment) extends Directive

case class ListDirective(name: Option[String], comment: Comment) extends Directive

case class NListDirective(name: Option[String], comment: Comment) extends Directive

case class NoCrossDirective(symbols: List[String], comment: Comment) extends Directive

case class OddDirective(comment: Comment) extends Directive

case class PackedDirective(decimal: String, maybeSymbol: Option[String], comment: Comment) extends Directive

case class PageDirective(comment: Comment) extends Directive

case class PSectDirective(name: String, arguments: Seq[String], comment: Comment) extends Directive

// TODO: Amend String to StringExpression
case class Rad50Directive(param: DelimitedString, comment: Comment) extends Directive

case class RadixDirective(param: String, comment: Comment) extends Directive

case class RemDirective(commentChar: String) extends Directive

case class RestoreDirective(comment: Comment) extends Directive

case class SaveDirective(comment: Comment) extends Directive

case class SbttlDirective(title: String) extends Directive

case class TitleDirective(title: String) extends Directive

case class WeakDirective(globals: List[String], comment: Comment) extends Directive

case class WordDirective(expressions: List[String], comment: Comment) extends Directive
