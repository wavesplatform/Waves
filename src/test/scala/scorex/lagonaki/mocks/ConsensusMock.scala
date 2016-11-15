package scorex.lagonaki.mocks

import scorex.consensus.nxt.NxtLikeConsensusModule
import scorex.settings.{Settings, WavesHardForkParameters}

class ConsensusMock extends NxtLikeConsensusModule(WavesHardForkParameters.Disabled) {
}
