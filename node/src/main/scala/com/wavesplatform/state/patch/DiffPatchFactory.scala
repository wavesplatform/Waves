package com.wavesplatform.state.patch

import com.wavesplatform.state.{Blockchain, Diff}

trait DiffPatchFactory {
  def isApplicable(b: Blockchain): Boolean = b.height == this.height
  def height: Int
  def apply(): Diff
}
