package scorex.consensus

import scorex.account.Account
import scorex.block.Block
import scorex.settings.ChainParameters
import scorex.transaction.{BalanceSheet, TransactionModule}

/**
 * Data and functions related to a Proof-of-Stake consensus algo
 */
trait PoSConsensusModule[ConsensusBlockData] extends ConsensusModule[ConsensusBlockData] {

  def forksConfig: ChainParameters
  
  def generatingBalance[TransactionalBlockData](account: Account, atHeight: Option[Int] = None)
                                               (implicit transactionModule: TransactionModule[TransactionalBlockData]): Long = {
    val balanceSheet = transactionModule.blockStorage.state.asInstanceOf[BalanceSheet]
    val generatingBalanceDepth = if (atHeight.exists(h => h >= forksConfig.generatingBalanceDepthFrom50To1000AfterHeight)) 1000 else 50
    balanceSheet.effectiveBalanceWithConfirmations(account, generatingBalanceDepth, atHeight)
  }

  override def generators(block: Block): Seq[Account]
}
