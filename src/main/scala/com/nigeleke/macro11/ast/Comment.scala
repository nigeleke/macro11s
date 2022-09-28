package com.nigeleke.macro11.ast

/** The comment field normally begins in column 33 and extends through the end of the line. This field is optional and may contain
  * any ASCII characters except null, RUBOUT, carriage-return, line-feed, vertical-tab or form-feed. All other characters appearing
  * in the comment field, even special characters reserved for use in MACRO-11, are checked only for ASCII legality and then
  * included in the assembly listing as they appear in the source text.
  *
  * All comment fields must begin with a semicolon (;). When lengthy comments extend beyond the end of the source line (column 80),
  * the comment may be resumed in a following line. Such a line must contain a leading semicolon, and it is suggested that the body
  * of the comment be continued in the same columnar position in which the comment began. A comment line can also be included as an
  * entirely separate line within the code body.
  *
  * Comments do not affect assembly processing or program execution. However, comments are necessary in source listings for later
  * analysis, debugging, or documentation purposes.
  *
  * @param comment
  *   The text to end-of-line; inclusive semi-colon, exclusive end-of-line.
  */
final case class Comment(comment: String)
