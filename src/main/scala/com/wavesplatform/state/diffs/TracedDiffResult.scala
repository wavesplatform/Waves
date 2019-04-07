package com.wavesplatform.state.diffs

import com.wavesplatform.state.Diff
import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.smart.script.ScriptExecutionTrace

case class TracedDiffResult(
    diffE: Either[ValidationError, Diff],
    trace: ScriptExecutionTrace = ScriptExecutionTrace()
)

object TracedDiffResult {
  implicit def wrap(diffE: Either[ValidationError, Diff]): TracedDiffResult = TracedDiffResult(diffE)
}
