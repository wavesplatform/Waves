package com.wavesplatform.consensus

import scorex.consensus.nxt.NxtLikeConsensusModule
import com.wavesplatform.settings.Constants

/**
  * Waves Consensus module
  */
class WavesConsensusModule extends NxtLikeConsensusModule(Constants.AvgBlockDelay) {
  override val InitialBaseTarget = 153722867L // for compatibility reason
}
