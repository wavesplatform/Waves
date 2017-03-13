package scorex.lagonaki.mocks

import scorex.consensus.nxt.WavesConsensusModule
import scorex.settings.TestBlockchainSettings

import scala.concurrent.duration._

class ConsensusMock extends WavesConsensusModule(TestBlockchainSettings.Disabled) {
}
