package com.wavesplatform.state.patch

import com.wavesplatform.state.Diff

trait DiffPatchFactory {
  def height: Int
  def apply(): Diff
}
