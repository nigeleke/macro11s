package com.nigeleke.macro11.ast

trait Label(val symbol: String)
case class GlobalLabel(override val symbol: String) extends Label(symbol)
case class LocalLabel(override val symbol: String)  extends Label(symbol)
