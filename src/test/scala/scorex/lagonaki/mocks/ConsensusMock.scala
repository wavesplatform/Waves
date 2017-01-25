package scorex.lagonaki.mocks

import scorex.consensus.nxt.WavesConsensusModule
import scorex.settings.{Settings, ChainParameters}

class ConsensusMock extends WavesConsensusModule(ChainParameters.Disabled) {
}
