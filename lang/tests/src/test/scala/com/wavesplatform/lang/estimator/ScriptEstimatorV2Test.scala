package com.wavesplatform.lang.estimator

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.utils.*
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2

class ScriptEstimatorV2Test extends ScriptEstimatorTestBase(ScriptEstimatorV2) {
  property("transitive ref usage") {
    def estimateRefUsage(ref: String): Long = {
      val script = s"""
                      |  let me = addressFromStringValue("")
                      |  func get() = getStringValue($ref, "")
                      |  get() + get() + get() + get() + get()
                      |
                    """.stripMargin

      val expr = compile(script)
      estimate(functionCosts(V3), expr).explicitGet()
    }

    val addressFromStrComplexity = 124
    estimateRefUsage("me") - estimateRefUsage("this") shouldBe addressFromStrComplexity + 1
  }

  property("transitive ref usage with overlapping") {
    val firstCallCount = 6

    val script =
      s"""
         |  let me = 1 + 1 + 1 + 1
         |  func third(p: Int) = me
         |  func second(me: Int) = third(me)
         |  func first() = second(1)
         |  ${List.fill(firstCallCount)("first()").mkString(" + ")}
         |
       """.stripMargin

    val expr = compile(script)

    val firstCallCost =
      5 /* call second()                     */ +
        1 /* eval 1                            */ +
        3 /* eval param me and pass to third() */ +
        5 /* call third()                      */ +
        2 /* eval ref me                       */

    estimate(functionCosts(V3), expr).explicitGet() shouldBe
      firstCallCost * firstCallCount +
        firstCallCount - 1 /* pluses */ +
        5 * 4 /* all blocks */ +
        7 /* calc 1 + 1 + 1 + 1 exactly once */
  }

  property("interrupting") {
    val count = 30
    val hangingScript =
      s"""
         | func f0() = 0
         | ${(1 to count).map(i => s"func f$i() = if (true) then f${i - 1}() else f${i - 1}()").mkString("\n")}
         | f$count()
       """.stripMargin

    @volatile var r: Either[String, Long] = Right(0)
    val run: Runnable = { () =>
      r = estimate(functionCosts(V3), compile(hangingScript))
    }
    val t = new Thread(run)
    t.setDaemon(true)

    t.start()
    Thread.sleep(5000)
    t.interrupt()
    Thread.sleep(500)

    r shouldBe Left("Script estimation was interrupted")
    t.getState shouldBe Thread.State.TERMINATED
  }
}
