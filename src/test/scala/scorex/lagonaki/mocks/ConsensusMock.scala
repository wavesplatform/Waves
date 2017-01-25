package scorex.lagonaki.mocks

import scorex.consensus.nxt.WavesConsensusModule
import scorex.settings.{Settings, WavesHardForkParameters}

class ConsensusMock extends WavesConsensusModule(WavesHardForkParameters.Disabled) {
}
