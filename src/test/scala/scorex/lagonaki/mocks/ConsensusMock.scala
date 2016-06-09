package scorex.lagonaki.mocks

import scorex.account.PublicKeyAccount
import scorex.consensus.nxt.{NxtLikeConsensusBlockData, NxtLikeConsensusModule}
import scorex.transaction.TransactionModule

class ConsensusMock extends NxtLikeConsensusModule {

  override val generatingBalanceDepth: Int = 0

}
