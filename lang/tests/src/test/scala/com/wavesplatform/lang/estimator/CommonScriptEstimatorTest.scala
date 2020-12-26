package com.wavesplatform.lang.estimator

import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.utils.functionCosts
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.estimator.ScriptEstimatorV1
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.evaluator.FunctionIds.SUM_LONG

class CommonScriptEstimatorTest extends ScriptEstimatorTestBase(ScriptEstimatorV1, ScriptEstimatorV2, ScriptEstimatorV3, evaluatorV2AsEstimator) {
  property("context leak") {
    def script(ref: String) =
      compile {
        s"""
           |  func inc($ref: Int) = $ref + 1
           |  let xxx = 5
           |  inc(xxx)
         """.stripMargin
      }
    estimateDelta(script("xxx"), script("y")) shouldBe Right(0)
  }

  property("func forward reference") {
    val expr = BLOCK(
      FUNC("f", Nil, FUNCTION_CALL(User("g"), Nil)),
      BLOCK(
        FUNC("g", Nil, CONST_LONG(1)),
        CONST_LONG(1)
      )
    )
    estimate(functionCosts(V3), expr) shouldBe Symbol("left")
  }

  property("func decl overlapping inside let") {
    def expr(outerFunc: String) =
      BLOCK(
        FUNC(outerFunc, Nil, FUNCTION_CALL(Native(SUM_LONG), List(CONST_LONG(1), CONST_LONG(1)))),
        BLOCK(
          LET(
            "b",
            BLOCK(
              FUNC("f", Nil, CONST_LONG(1)),
              FUNCTION_CALL(User("f"), Nil)
            )
          ),
          FUNCTION_CALL(Native(SUM_LONG), List(FUNCTION_CALL(User(outerFunc), Nil), REF("b")))
        )
      )

    /*
      func f() = 1 + 1
      let b = {
        func f() = 1
        1
      }
      f() + b
     */
    estimateDelta(expr("f"), expr("g")) shouldBe Right(0)
  }

  property("func decl overlapping inside func") {
    def expr(outerFunc: String) =
      BLOCK(
        FUNC(outerFunc, Nil, FUNCTION_CALL(Native(SUM_LONG), List(CONST_LONG(1), CONST_LONG(1)))),
        BLOCK(
          FUNC(
            "b",
            Nil,
            BLOCK(
              FUNC("f", Nil, CONST_LONG(1)),
              FUNCTION_CALL(User("f"), Nil)
            )
          ),
          FUNCTION_CALL(
            Native(SUM_LONG),
            List(FUNCTION_CALL(User(outerFunc), Nil), FUNCTION_CALL(User("b"), Nil))
          )
        )
      )

    /*
      func f() = 1 + 1
      func b() = {
        func f() = 1
        1
      }
      f() + b()
     */
    estimateDelta(expr("f"), expr("g")) shouldBe Right(0)
  }

  property("func decl overlapping inside condition") {
    def expr(outerFunc: String) =
      BLOCK(
        FUNC(outerFunc, Nil, FUNCTION_CALL(Native(SUM_LONG), List(CONST_LONG(1), CONST_LONG(1)))),
        FUNCTION_CALL(
          Native(SUM_LONG),
          List(
            IF(
              TRUE,
              BLOCK(
                FUNC("f", Nil, CONST_LONG(1)),
                FUNCTION_CALL(User("f"), Nil)
              ),
              CONST_LONG(1)
            ),
            FUNCTION_CALL(User(outerFunc), Nil)
          )
        )
      )

    /*
      func f() = 1 + 1
      (
        if (true)
        then {
          func f() = 1
          1
        }
        else 1
      ) + f()
     */
    estimateDelta(expr("f"), expr("g")) shouldBe Right(0)
  }

  property("func decl overlapping inside func parameter") {
    def expr(outerFunc: String) =
      BLOCK(
        FUNC(outerFunc, Nil, FUNCTION_CALL(Native(SUM_LONG), List(CONST_LONG(1), CONST_LONG(1)))),
        BLOCK(
          FUNC("b", List("a"), REF("a")),
          FUNCTION_CALL(
            Native(SUM_LONG),
            List(
              FUNCTION_CALL(
                User("b"),
                List(
                  BLOCK(
                    FUNC("f", Nil, CONST_LONG(1)),
                    FUNCTION_CALL(User("f"), Nil)
                  )
                )
              ),
              FUNCTION_CALL(User(outerFunc), Nil)
            )
          )
        )
      )

    /*
      func f() = 1 + 1
      func b(a) = a
      b({func f() = 1; f()}) + f()
     */
    estimateDelta(expr("f"), expr("g")) shouldBe Right(0)
  }

  property("let overlapping") {
    def expr(outerLet: String): EXPR =
      BLOCK(
        LET(outerLet, FUNCTION_CALL(Native(SUM_LONG), List(CONST_LONG(1), CONST_LONG(1)))),
        BLOCK(
          LET(
            "b",
            BLOCK(
              LET("a", CONST_LONG(1)),
              CONST_LONG(1)
            )
          ),
          FUNCTION_CALL(Native(SUM_LONG), List(REF(outerLet), REF("b")))
        )
      )

    /*
      let a = 1 + 1
      let b = {
        let a = 1
        1
      }
      a + b
     */
    estimateDelta(expr("a"), expr("x")) shouldBe Right(0)
  }
}
