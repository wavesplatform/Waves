package com.wavesplatform.lang

import com.wavesplatform.lang.Context.CustomFunction
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks

import scala.util.Try

class CompleteTest extends PropSpec with PropertyChecks with Matchers with NoShrink {

  val multiplierFunction = CustomFunction("MULTIPLY", Terms.INT, List(("x1", Terms.INT), ("x2", Terms.INT))) {
    case x1 :: x2 :: Nil => Try { x1.asInstanceOf[Int] * x2.asInstanceOf[Int] }.toEither.left.map(_.toString)
  }

  private def eval(code: String) = {
    val untyped = Parser(code).get.value
    val typed   = TypeChecker(TypeChecker.TypeCheckerContext.empty, untyped)
    typed.flatMap(Evaluator(Context(Map.empty, Map.empty, Map(multiplierFunction.name -> multiplierFunction)), _))
  }

  property("pattern matching") {
    eval("""
           |let MULTICHARVARNAME = Some(500)
           |
           |let Z = match(MULTICHARVARNAME) {
           | case None => 8
           | case Some(B) => B + B
           | }
           |
           | get(Some(Z)) + 1
           |
      """.stripMargin) shouldBe Right(1001)

    eval("""
           |
           | let X = Some(10)
           |
           |match(X) {
           |  case None => 0
           |  case Some(V) => V + V + V + V
           |}
         """.stripMargin) shouldBe Right(40)

    eval("""
           |
           |let X = Some(10)
           |
           |match(X) {
           |  case Some(V) => V + V + V + V
           |  case None => 0
           |}
         """.stripMargin) shouldBe Right(40)
  }

  property("function call") {
    eval(
      """
        |
        | MULTIPLY(3,4)
        |
      """.stripMargin) shouldBe Right(12)

  }
}
