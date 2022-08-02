package com.wavesplatform.api.common
import com.wavesplatform.state.{Diff, Height}

trait UseLiquidDiff {
  def apply[A](f: Option[(Height, Diff)] => A): A
}
