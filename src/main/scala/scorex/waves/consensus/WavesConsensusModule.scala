package scorex.waves.consensus

import scorex.consensus.nxt.NxtLikeConsensusModule
import scorex.waves.settings.Constants

/**
  * Waves Consensus module
  */
class WavesConsensusModule extends NxtLikeConsensusModule(Constants.AvgBlockDelay) {
  val InitialBaseTarget = 153722867L // for compatibility reason
}
