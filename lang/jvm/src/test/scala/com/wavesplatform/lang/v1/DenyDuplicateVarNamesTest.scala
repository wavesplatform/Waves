package com.wavesplatform.lang.v1

import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.Version.ExprV1
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.testing.ScriptGen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class DenyDuplicateVarNamesTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  val test = DenyDuplicateVarNames(ExprV1, Set("height"), _: EXPR)

  property("allow $ duplicates")(test(LET_BLOCK(LET("$x", TRUE), LET_BLOCK(LET("$x", TRUE), TRUE))) shouldBe 'right)

  property("deny overwrite height")(DenyDuplicateVarNames(ExprV1, Set("height"), LET_BLOCK(LET("height", TRUE), TRUE)) should produce("height"))

  property("deny duplicates in block")(test(LET_BLOCK(LET("x", TRUE), LET_BLOCK(LET("x", TRUE), TRUE))) should produce("x"))

  property("deny @ args")(test(LET_BLOCK(LET("@a", TRUE), TRUE)) should produce("@"))

  property("deny duplicates in if cond")(test(IF(LET_BLOCK(LET("x", TRUE), TRUE), LET_BLOCK(LET("x", TRUE), TRUE), TRUE)) should produce("x"))

  property("deny duplicates in if branch")(test(IF(TRUE, LET_BLOCK(LET("x", TRUE), TRUE), LET_BLOCK(LET("x", TRUE), TRUE))) should produce("x"))

  property("deny duplicates in funcitoncall")(
    test(FUNCTION_CALL(FunctionHeader.User("foo"), List(LET_BLOCK(LET("x", TRUE), TRUE), LET_BLOCK(LET("x", TRUE), TRUE)))) should produce("x"))

}
