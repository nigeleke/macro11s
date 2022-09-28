package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** The listing control directives may be used without arguments, in which case the listing directives alter the listing level
  * count. The listing level count is initialized to zero. At each occurrence of a [[LIST]] directive, the listing level count is
  * incremented; at each occurrence of an [[NLIST]] directive, the listing level count is decremented. When the level count is
  * negative, the listing is suppressed (unless the line contains an error).
  *
  * @param name
  * @todo
  *   change string to specific list options
  * @param comment
  *   The comment at the end of the directive.
  * @see
  *   [[NLIST]]
  */
final case class LIST(name: Option[String], comment: Comment) extends Directive
