package com.wavesplatform.lang.evaluator

import com.wavesplatform.lang.CommonError
import com.wavesplatform.lang.v1.evaluator.EvaluationResult
import com.wavesplatform.test.PropSpec
import monix.eval.Coeval

class EvaluationResultTest extends PropSpec {
  private val failure = new IllegalStateException("boom")

  property("an error skips the following flatMap and map steps but not transform") {
    var reached = false
    val result = EvaluationResult[Int]("failed", 7)
      .flatMap { v =>
        reached = true
        EvaluationResult(v + 1)
      }
      .map(_ * 2)
      .transform(_.left.map { case (e, limit) => (e, limit + 1) })
    result.run() shouldBe Left((CommonError("failed"), 8))
    reached shouldBe false
  }

  property("a thrown exception unwinds to the nearest handler and evaluation goes on after it") {
    val result = EvaluationResult(1)
      .flatMap(_ => EvaluationResult.delay[Int](throw failure))
      .map(_ + 100)
      .handleErrorWith(e => EvaluationResult.fromEither(Left((CommonError(e.getMessage), 3))))
      .transform(_.left.map { case (e, limit) => (e, limit * 2) })
    result.run() shouldBe Left((CommonError("boom"), 6))
  }

  property("an unhandled exception is thrown from run") {
    val result = EvaluationResult(1).map[Int](_ => throw failure).map(_ + 1)
    the[IllegalStateException] thrownBy result.run() shouldBe failure
  }

  property("a failing Coeval reaches the handler") {
    val result = EvaluationResult
      .fromCoeval[Int](Coeval.raiseError(failure))
      .handleErrorWith(_ => EvaluationResult(42))
    result.run() shouldBe Right(42)
  }

  property("deeply nested steps run without overflowing the stack") {
    val depth      = 1_000_000
    val leftNested = (1 to depth).foldLeft(EvaluationResult(0))((r, _) => r.flatMap(v => EvaluationResult(v + 1)))
    leftNested.run() shouldBe Right(depth)

    def rightNested(n: Int): EvaluationResult[Int] =
      if (n == 0) EvaluationResult(0) else EvaluationResult.defer(rightNested(n - 1)).map(_ + 1)
    rightNested(depth).run() shouldBe Right(depth)
  }

  property("a description can be run more than once, re-evaluating delayed steps") {
    var evaluations = 0
    val result      = EvaluationResult.delay[Int] { evaluations += 1; Right(evaluations) }.map(_ * 10)
    result.run() shouldBe Right(10)
    result.run() shouldBe Right(20)
  }
}
