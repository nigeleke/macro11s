package com.nigeleke.macro11.ast

case class DelimitedString(delimiter: Char, content: String)

object DelimitedString:
  def apply(value: String): DelimitedString =
    require(value.length >= 2)
    require(value.head == value.last)
    DelimitedString(value.head, value.substring(1, value.length - 1))
