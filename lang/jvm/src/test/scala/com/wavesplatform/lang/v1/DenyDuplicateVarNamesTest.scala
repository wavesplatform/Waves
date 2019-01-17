package com.wavesplatform.lang.v1

import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.Version.V1
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.testing.ScriptGen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class DenyDuplicateVarNamesTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  val test = DenyDuplicateVarNames(V1, Set("height"), _: EXPR)

  property("allow $ duplicates")(test(BLOCKV1(LET("$x", TRUE), BLOCKV1(LET("$x", TRUE), TRUE))) shouldBe 'right)

  property("deny overwrite height")(DenyDuplicateVarNames(V1, Set("height"), BLOCKV1(LET("height", TRUE), TRUE)) should produce("height"))

  property("deny duplicates in block")(test(BLOCKV1(LET("x", TRUE), BLOCKV1(LET("x", TRUE), TRUE))) should produce("x"))

  property("deny @ args")(test(BLOCKV1(LET("@a", TRUE), TRUE)) should produce("@"))

  property("deny duplicates in if cond")(test(IF(BLOCKV1(LET("x", TRUE), TRUE), BLOCKV1(LET("x", TRUE), TRUE), TRUE)) should produce("x"))

  property("deny duplicates in if branch")(test(IF(TRUE, BLOCKV1(LET("x", TRUE), TRUE), BLOCKV1(LET("x", TRUE), TRUE))) should produce("x"))

  property("deny duplicates in funcitoncall")(
    test(FUNCTION_CALL(FunctionHeader.User("foo"), List(BLOCKV1(LET("x", TRUE), TRUE), BLOCKV1(LET("x", TRUE), TRUE)))) should produce("x"))

}
