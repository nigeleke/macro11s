package com.nigeleke.macro11.ast

trait Directive

// TODO: Amend String to StringExpression
case class AsciiDirective(param: String, maybeComment: Comment) extends Directive

case class AscizDirective(param: String, maybeComment: Comment) extends Directive

case class ASectDirective(maybeComment: Comment) extends Directive

// TODO: Amend String to NumericExpression
case class BlkbDirective(expression: String, maybeComment: Comment) extends Directive

case class BlkwDirective(expression: String, maybeComment: Comment) extends Directive

case class ByteDirective(expressions: List[String], maybeComment: Comment) extends Directive

case class CrossDirective(symbols: List[String], maybeComment: Comment) extends Directive

case class CSectDirective(name: String, maybeComment: Comment) extends Directive

case class DsablDirective(argument: EnablDsablArgument, maybeComment: Comment) extends Directive

case class EnablDirective(argument: EnablDsablArgument, maybeComment: Comment) extends Directive

case class EndDirective(expression: String, maybeComment: Comment) extends Directive

case class EndcDirective(maybeComment: Comment) extends Directive

case class EvenDirective(maybeComment: Comment) extends Directive

case class Flt2Directive(floats: List[BigDecimal], maybeComment: Comment) extends Directive

case class Flt4Directive(floats: List[BigDecimal], maybeComment: Comment) extends Directive

case class GlobalDirective(globals: List[String], maybeComment: Comment) extends Directive

case class IdentDirective(content: String, maybeComment: Comment) extends Directive

case class IfDirective(condition: String, arguments: List[String], maybeComment: Comment) extends Directive

case class IffDirective(maybeComment: Comment) extends Directive

case class IftDirective(maybeComment: Comment) extends Directive

case class IftfDirective(maybeComment: Comment) extends Directive

case class IifDirective(
    condition: String,
    arguments: List[String],
    instruction: Instruction,
    maybeComment: Comment
) extends Directive

case class IncludeDirective(file: String, maybeComment: Comment) extends Directive

case class LibraryDirective(file: String, maybeComment: Comment) extends Directive

case class LimitDirective(maybeComment: Comment) extends Directive

case class ListDirective(name: Option[String], maybeComment: Comment) extends Directive

case class NListDirective(name: Option[String], maybeComment: Comment) extends Directive

case class NoCrossDirective(symbols: List[String], maybeComment: Comment) extends Directive

case class OddDirective(maybeComment: Comment) extends Directive

case class PackedDirective(decimal: String, maybeSymbol: Option[String], maybeComment: Comment) extends Directive

case class PageDirective(maybeComment: Comment) extends Directive

case class PSectDirective(name: String, arguments: Seq[String], maybeComment: Comment) extends Directive

// TODO: Amend String to StringExpression
case class Rad50Directive(param: String, maybeComment: Comment) extends Directive

case class RadixDirective(param: String, maybeComment: Comment) extends Directive

case class RemDirective(commentChar: String) extends Directive

case class RestoreDirective(maybeComment: Comment) extends Directive

case class SaveDirective(maybeComment: Comment) extends Directive

case class SbttlDirective(title: String) extends Directive

case class TitleDirective(title: String) extends Directive

case class WeakDirective(globals: List[String], maybeComment: Comment) extends Directive

case class WordDirective(expressions: List[String], maybeComment: Comment) extends Directive
