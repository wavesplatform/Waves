package scorex.lagonaki

import scorex.account.PrivateKeyAccount
import scorex.app.RunnableApplication
import scorex.block.Block
import scorex.transaction.{History, Transaction, TransactionsBlockField}
import scorex.utils.ScorexLogging

import scala.util.Random

trait TransactionTestingCommons extends scorex.waves.TestingCommons with ScorexLogging {
  implicit lazy val consensusModule = application.consensusModule
  implicit lazy val transactionModule = application.transactionModule

  if (application.transactionModule.blockStorage.history.isEmpty) {
    application.transactionModule.blockStorage.appendBlock(Block.genesis(RunnableApplication.consensusGenesisBlockData,transactionModule.genesisData))
  }

  if (application.wallet.privateKeyAccounts().size < 3) {
    application.wallet.generateNewAccounts(3)
  }

  val ab = applicationNonEmptyAccounts.map(application.consensusModule.generatingBalance(_)).sum
  require(ab > 2)

  def applicationNonEmptyAccounts: Seq[PrivateKeyAccount] =
    application.wallet.privateKeyAccounts().filter(application.consensusModule.generatingBalance(_) > 0)

  def applicationEmptyAccounts: Seq[PrivateKeyAccount] =
    application.wallet.privateKeyAccounts().filter(application.consensusModule.generatingBalance(_) == 0)

  def genValidBlock(): Block = {
    application.consensusModule.generateNextBlocks(applicationNonEmptyAccounts)(application.transactionModule).headOption match {
        // TODO: isBlockValid is a method of actor, need to refactor that?
      case Some(block: Block) => block // if application.coordinator.isBlockValid(block) => block
      case None =>
        Thread.sleep(500)
        genValidBlock()
    }
  }

  def genValidTransaction(randomAmnt: Boolean = true,
                          recipientOpt: Option[PrivateKeyAccount] = None,
                          senderOpt: Option[PrivateKeyAccount] = None
                         ): Transaction = {
    val senderAcc = senderOpt.getOrElse(randomFrom(applicationNonEmptyAccounts))
    val senderBalance = application.consensusModule.generatingBalance(senderAcc) / 1000
    require(senderBalance > 0)
    val fee = Random.nextInt(5).toLong + 1
    if (senderBalance <= fee) {
      genValidTransaction(randomAmnt, recipientOpt, senderOpt)
    } else {
      val amt = if (randomAmnt) Math.abs(Random.nextLong() % (senderBalance - fee))
      else senderBalance - fee
      val recipient = recipientOpt.getOrElse(randomFrom(applicationNonEmptyAccounts))
      val tx = application.transactionModule.createPayment(senderAcc, recipient, amt, fee).right.get
      if (application.transactionModule.blockStorage.state.isValid(tx, tx.timestamp)) tx
      else genValidTransaction(randomAmnt, recipientOpt, senderOpt)
    }
  }

  def includedTransactions(b: Block, history: History): Seq[Transaction] = {
    if (b.transactionDataField.asInstanceOf[TransactionsBlockField].value.isEmpty) includedTransactions(history.parent(b).get, history)
    else b.transactionDataField.asInstanceOf[TransactionsBlockField].value
  }

}
