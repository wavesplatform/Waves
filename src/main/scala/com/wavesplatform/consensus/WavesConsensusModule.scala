package com.wavesplatform.consensus

import com.wavesplatform.settings.Constants
import scorex.consensus.nxt.NxtLikeConsensusModule
import scorex.settings.WavesHardForkParameters

/**
  * Waves Consensus module
  */
class WavesConsensusModule(forkParameters: WavesHardForkParameters) extends NxtLikeConsensusModule(forkParameters, Constants.AvgBlockDelay) {
  override val InitialBaseTarget = 153722867L // for compatibility reason
}
