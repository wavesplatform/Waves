package com.wavesplatform.state.diffs

import cats.kernel.Semigroup
import com.wavesplatform.state.Diff
import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.smart.script.TraceStep
import cats.implicits._

case class TracedDiffResult(
    diffE: Either[ValidationError, Diff],
    trace: List[TraceStep] = Nil
)

object TracedDiffResult {
  implicit def wrapE(diffE: Either[ValidationError, Diff]): TracedDiffResult = TracedDiffResult(diffE)
  implicit def wrapDiff(diff: Diff): TracedDiffResult = TracedDiffResult(Right(diff))

  implicit val tracedDiffResultSemigroup: Semigroup[TracedDiffResult] = (a, b) =>
    TracedDiffResult(
      a.diffE |+| b.diffE,
      a.trace |+| b.trace
  )
}
