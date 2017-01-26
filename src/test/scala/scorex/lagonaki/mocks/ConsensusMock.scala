package scorex.lagonaki.mocks

import scorex.consensus.nxt.WavesConsensusModule
import scorex.settings.{Settings, ChainParameters}
import scala.concurrent.duration._

class ConsensusMock extends WavesConsensusModule(ChainParameters.Disabled, 5.seconds) {
}
