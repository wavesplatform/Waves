package scorex.lagonaki.mocks

import scorex.account.PublicKeyAccount
import scorex.block.Block
import scorex.consensus.nxt.NxtLikeConsensusModule
import scorex.transaction.TransactionModule

class ConsensusMock extends NxtLikeConsensusModule {

  override val generatingBalanceDepth: Int = 0

  override def nextBlockGenerationTime[TT](lastBlock: Block, account: PublicKeyAccount)
                                          (implicit tm: TransactionModule[TT]): Option[Long] = None
}
