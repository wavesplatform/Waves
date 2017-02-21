package scorex.consensus

import scorex.account.Account
import scorex.settings.ChainParameters
import scorex.transaction.TransactionModule

trait PoSConsensusModule extends ConsensusModule {

  def forksConfig: ChainParameters
  
  def generatingBalance(account: Account, atHeight: Option[Int] = None)
                                               (implicit transactionModule: TransactionModule): Long = {
    val balanceSheet = transactionModule.blockStorage.state
    val generatingBalanceDepth = if (atHeight.exists(h => h >= forksConfig.generatingBalanceDepthFrom50To1000AfterHeight)) 1000 else 50
    balanceSheet.effectiveBalanceWithConfirmations(account, generatingBalanceDepth, atHeight)
  }

}
