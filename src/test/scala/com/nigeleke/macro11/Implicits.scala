package com.nigeleke.macro11

import org.scalatest.*
import org.scalacheck.*

type AssertionToProp = Assertion => Prop
given AssertionToProp = (a: Assertion) =>
  a match
    case Succeeded => Prop.passed
    case Failed(_) => Prop.falsified
