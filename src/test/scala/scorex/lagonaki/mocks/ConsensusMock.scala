package scorex.lagonaki.mocks

import scorex.consensus.nxt.NxtLikeConsensusModule

class ConsensusMock extends NxtLikeConsensusModule {

  override val generatingBalanceDepth: Int = 0

}
