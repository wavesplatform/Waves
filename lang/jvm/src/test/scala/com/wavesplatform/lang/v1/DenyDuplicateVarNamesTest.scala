package com.wavesplatform.lang.v1

import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.testing.ScriptGen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class DenyDuplicateVarNamesTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  val test = DenyDuplicateVarNames(Set("height"), _: EXPR)

  property("allow $ duplicates")(test(BLOCK(LET("$x", TRUE), BLOCK(LET("$x", TRUE), TRUE))) shouldBe 'right)

  property("deny overwrite height")(DenyDuplicateVarNames(Set("height"), BLOCK(LET("height", TRUE), TRUE)) should produce("height"))

  property("deny duplicates in block")(test(BLOCK(LET("x", TRUE), BLOCK(LET("x", TRUE), TRUE))) should produce("x"))

  property("deny @ args")(test(BLOCK(LET("@a", TRUE), TRUE)) should produce("@"))

  property("deny duplicates in if cond")(test(IF(BLOCK(LET("x", TRUE), TRUE), BLOCK(LET("x", TRUE), TRUE), TRUE)) should produce("x"))

  property("deny duplicates in if branch")(test(IF(TRUE, BLOCK(LET("x", TRUE), TRUE), BLOCK(LET("x", TRUE), TRUE))) should produce("x"))

  property("deny duplicates in funcitoncall")(
    test(FUNCTION_CALL(FunctionHeader.User("foo"), List(BLOCK(LET("x", TRUE), TRUE), BLOCK(LET("x", TRUE), TRUE)))) should produce("x"))

}
