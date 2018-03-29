package com.wavesplatform.state2.diffs

import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.settings.FunctionalitySettings
import scorex.settings.TestFunctionalitySettings

package object smart {
  val smartEnabledFS: FunctionalitySettings =
    TestFunctionalitySettings.Enabled.copy(
      preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0, BlockchainFeatures.DataTransaction.id -> 0))
}
