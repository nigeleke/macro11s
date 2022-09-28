package com.nigeleke.macro11.ast.directives

import com.nigeleke.macro11.ast.*

/** The [[PAGE]] directive is used within the source program to perform a page eject at desired points in the listing. This
  * directive takes no arguments and causes a skip to the top of the next page when encountered. It also causes the page number to
  * be incremented and the line sequence counter to be cleared. The [[PAGE]] directive does not appear in the listing. When used
  * within a macro definition, the [[PAGE]] directive is ignored during the assembly of the macro definition. Rather, the page eject
  * operation is performed as the macro itself is expanded. In this case, the page number is also incremented.
  *
  * @param comment
  *   The comment at the end of the directive.
  */
final case class PAGE(comment: Comment) extends Directive
