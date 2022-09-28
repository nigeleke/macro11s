package com.nigeleke.macro11.ast

import scala.annotation.tailrec

/** A [[Label]] is a user-defined symbol which is assigned the value of the current location counter and entered into the
  * user-defined symbol table. The current location counter is used by MACRO-11 to assign memory addresses to the source program
  * statements as they are encountered during the assembly process. Thus, a [[Label]] is a means of symbolically referring to a
  * specific statement.
  *
  * @param symbol
  *   The symbol for the user defined symbol table.
  * @param isGlobal
  *   True if the label was terminated by "::", false otherwise. If isGlobal is false, the symbol itself may still be global.
  */
case class Label(symbol: String, isGlobal: Boolean)

object Label:

  /** Create a Label from a string,
    * @constructor
    * @param s
    *   The [[Label]] text, which must be terminated by one, or two ":" characters.
    * @return
    *   The[[Label]] object.
    */
  def apply(s: String): Label =
    require(s.endsWith(":"))
    Label(s.stripTrailing(":"), s.endsWith(("::")))

extension (s: String)
  @tailrec
  def stripTrailing(tail: String): String =
    if !s.endsWith(tail)
    then s
    else s.take(s.length - tail.length).stripTrailing(tail)
