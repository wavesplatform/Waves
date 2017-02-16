package scorex.lagonaki.mocks

import scorex.consensus.nxt.WavesConsensusModule
import scorex.settings.ChainParameters

import scala.concurrent.duration._

class ConsensusMock extends WavesConsensusModule(ChainParameters.Disabled, 5.seconds) {
}
