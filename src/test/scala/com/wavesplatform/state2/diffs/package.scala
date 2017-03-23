package com.wavesplatform.state2

import scorex.account.Account

package object diffs {
  def ensureSenderHasEnoughBalance(s: StateWriter)(sender: Account, assets: List[ByteArray]): Unit = {
    s.applyBlockDiff(Diff(Map.empty,
      Map(sender -> Portfolio(Long.MaxValue - 1, Long.MaxValue - 1, assets.map(a => a -> (Long.MaxValue - 1)).toMap)),
      assets.map(a => a -> AssetInfo(isReissuable = true, Long.MaxValue - 1)).toMap
    ).asBlockDiff)

  }

}
