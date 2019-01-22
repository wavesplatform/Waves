package com.wavesplatform.lang.compiler

import com.wavesplatform.lang.v1.compiler.{Decompiler, Terms}
import com.wavesplatform.lang.v1.compiler.Terms._
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks

class DecompilerTest extends PropSpec with PropertyChecks with Matchers {

  property("simple if") {
    val expr = IF(TRUE, CONST_LONG(1), CONST_STRING("XXX"))
    Decompiler(expr) shouldBe """if (true) then 1 else "XXX"""".stripMargin
  }

  property("simple let") {
    val expr = Terms.BLOCKV1(LET("a", CONST_LONG(1)), TRUE)
    Decompiler(expr) shouldBe("""let a = 1; true""".stripMargin)
  }
//
//  property("simple function") {
//    val expr = FUNC("alfa", List("a", "b"), IF(TRUE, CONST_LONG(1), CONST_STRING("XXX")))
//    Decompiler(expr) shouldBe("""let a = 1""".stripMargin)
//  }
//
//  property("simple function call") {
//    val expr = FUNCTION_CALL(
//      AnyPos,
//      PART.VALID(AnyPos, functionWithTwoPrarmsOfTheSameType.name),
//      List(CONST_LONG(AnyPos, 1), CONST_BYTEVECTOR(AnyPos, PART.VALID(AnyPos, ByteVector.empty)))
//    )
//    Decompiler(expr) shouldBe("""???""".stripMargin)
//  }

}