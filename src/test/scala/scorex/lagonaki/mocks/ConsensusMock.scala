package scorex.lagonaki.mocks

import scorex.account.PublicKeyAccount
import scorex.consensus.nxt.{NxtLikeConsensusBlockData, NxtLikeConsensusModule}
import scorex.transaction.TransactionModule

class ConsensusMock extends NxtLikeConsensusModule{
/*
  override protected def calcTarget(lbd: NxtLikeConsensusBlockData, lbt: Long, g: PublicKeyAccount)
                                   (implicit transactionModule: TransactionModule[_]): BigInt = {
      BigInt("99999999999999999999999999")
  }
*/
}
