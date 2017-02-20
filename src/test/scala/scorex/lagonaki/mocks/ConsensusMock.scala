package scorex.lagonaki.mocks

import scala.concurrent.duration._
import scorex.consensus.nxt.WavesConsensusModule
import scorex.settings.TestChainParameters

class ConsensusMock extends WavesConsensusModule(TestChainParameters.Disabled, 5.seconds) {
}
