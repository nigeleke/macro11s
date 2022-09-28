package com.nigeleke.macro11.ast

/** The vertical-tab, null, line-feed, RUBOUT, and all other non-printable ASCII characters, except carriage-return and form-feed,
  * cause an error code (I) if used. The carriage-return and form-feed characters are flagged with an error code (A) because these
  * characters end the scan of the line, preventing MACRO-11 from detecting the matching delimiter at the end of the character
  * string.
  *
  * Delimiters may be any paired printing characters, other than the equal sign (=), the left angle bracket (<), or the semicolon
  * (;), as long as the delimiting character is not contained within the text string itself. If the delimiting characters do not
  * match, or if an illegal delimiting character is used, an error code (A) is flagged.
  *
  * @param delimiter
  *   The delimiting ASCII printable, non-space, ;, = or <, character.
  * @param content
  *   The delimited string.
  */
final case class DelimitedString(delimiter: Char, content: String)

object DelimitedString:

  /** @constructor
    * @param value
    *   The value from which a DelimitedString will be created. The value must be at least characters long (the delimiters), with
    *   the first and last character representing the same character.
    * @return
    *   A valid DelimitiedString.
    */
  def from(value: String): DelimitedString =
    require(value.length >= 2)
    require(value.head == value.last)
    require(!List(";", "=", "<").contains(value.head))
    DelimitedString(value.head, value.substring(1, value.length - 1))
