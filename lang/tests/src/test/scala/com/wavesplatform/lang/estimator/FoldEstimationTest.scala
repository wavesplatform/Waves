package com.wavesplatform.lang.estimator

import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3

class FoldEstimationTest extends ScriptEstimatorTestBase(ScriptEstimatorV3(fixOverflow = true, overhead = true, letFixes = true)) {
  property("unique names are used for different folds") {
    estimate(
      """
        | func f1(acc: String, v: String) = acc + v
        | let src = ["a", "b", "c"]
        | FOLD<3>(src, "F1", f1) != FOLD<3>(src, "F2", f1)
      """.stripMargin
    ).isRight shouldBe true
  }

  property("unique names are used for folds inside foldFunc") {
    estimate(
      """
        | let src = [
        |   ["01", "v1_1", "v1_2"],
        |   ["02", "v2_1", "v2_2"],
        |   ["03", "v3_1", "v3_2"]
        | ]
        | func f1(acc: String, v: String) = acc + v
        | func f2(acc: String, v: List[String]) = acc + FOLD<3>(v, "STEP", f1)
        | FOLD<3>(src, "", f2) == "STEP01v1_1v1_2STEP02v2_1v2_2STEP03v3_1v3_2"
      """.stripMargin
    ).isRight shouldBe true
  }

  property("unique names are used for native fold") {
    estimate(
      """
        | func f1(acc: List[String], v: Int) = cons(toString(v), acc)
        | func add(acc: String, v: String) = acc + v
        | FOLD<3>(FOLD<3>([1, 2, 3], [], f1), "F1", add) == "F1321"
      """.stripMargin
    ).isRight shouldBe true
  }

  property("multiple scopes") {
    estimate(
      """
        | let foo = [1, 2, 3]
        | func add(a: Int, b: Int) = a + b
        |
        | func f(x: Int) = {
        |   let a = FOLD<3>(foo, 0, add)
        |   FOLD<3>(foo, a, add)
        | }
        |
        | f(10) > 0
      """.stripMargin
    ).isRight shouldBe true
  }
}
