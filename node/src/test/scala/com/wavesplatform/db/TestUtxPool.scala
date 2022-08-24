package com.wavesplatform.db
import com.wavesplatform.settings.UtxSettings
import com.wavesplatform.state.{Blockchain, Diff}
import com.wavesplatform.utils.Time
import com.wavesplatform.utx.UtxPoolImpl

class TestUtxPool(time: Time, blockchain: Blockchain, utxSettings: UtxSettings, isMiningEnabled: Boolean, beforeSetPriorityDiffs: () => Unit)
    extends UtxPoolImpl(time, blockchain, utxSettings, isMiningEnabled) {

  override def setPriorityDiffs(discDiffs: Seq[Diff]): Unit = {
    beforeSetPriorityDiffs()
    super.setPriorityDiffs(discDiffs)
  }
}
