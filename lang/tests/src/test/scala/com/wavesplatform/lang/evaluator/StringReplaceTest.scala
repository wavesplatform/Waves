package com.wavesplatform.lang.evaluator

import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BOOLEAN

class StringReplaceTest extends EvaluatorSpec {
  private implicit val version: StdLibVersion = V9

  property("successful results") {
    eval("""
      "AA".replaceFirst("", "B")     == "BAA" &&
      "AA".replaceFirst("", "")      == "AA" &&
      "AA".replaceFirst("A", "")     == "A" &&
      "AA".replaceFirst("A", "B")    == "BA" &&
      "AA".replaceFirst(".*", "B")   == "AA" &&
      "AB^AB$^AB$AB".replaceFirst("^AB$", "CD") == "ABCD^AB$AB"
    """) shouldBe Right(CONST_BOOLEAN(true))
  }
}
