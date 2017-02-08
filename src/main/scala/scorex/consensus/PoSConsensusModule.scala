package scorex.consensus

import scorex.account.Account
import scorex.block.Block
import scorex.settings.ChainParameters
import scorex.transaction.{BalanceSheet, TransactionModule}

/**
 * Data and functions related to a Proof-of-Stake consensus algo
 */
trait PoSConsensusModule extends ConsensusModule {

  def forksConfig: ChainParameters
  
  def generatingBalance(account: Account, atHeight: Option[Int] = None)
                                               (implicit transactionModule: TransactionModule): Long =
    transactionModule.blockStorage.state.asInstanceOf[BalanceSheet]
      .balanceWithConfirmations(account, if (atHeight.exists(h => h >= forksConfig.generatingBalanceDepthFrom50To1000AfterHeight)) 1000 else 50, atHeight)

}
