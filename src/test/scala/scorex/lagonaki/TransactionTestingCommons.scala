package scorex.lagonaki

import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scorex.lagonaki.TestingCommons._
import scorex.transaction.{History, GenesisTransaction, Transaction}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Random

trait TransactionTestingCommons extends TestingCommons {
  implicit val consensusModule = application.consensusModule
  implicit val transactionModule = application.transactionModule

  if (application.transactionModule.blockStorage.history.isEmpty) {
    application.transactionModule.blockStorage.appendBlock(Block.genesis())
  }

  if (application.wallet.privateKeyAccounts().size < 3) application.wallet.generateNewAccounts(3)

  def accounts = application.wallet.privateKeyAccounts()
    .filter(a => application.consensusModule.generatingBalance(a) > 0)

  val ab = accounts.map(application.consensusModule.generatingBalance(_)).sum
  require(ab > 2)

  def genValidBlock(): Block = {
    Await.result(application.consensusModule.generateNextBlocks(accounts)(application.transactionModule), 20.seconds).headOption match {
      case Some(block: Block) if block.isValid => block
      case None =>
        Thread.sleep(500)
        genValidBlock()
    }
  }

  val genesisAccs = application.blockStorage.history.genesis.transactions.flatMap(_ match {
    case gtx: GenesisTransaction => Some(gtx.recipient)
    case _ => None
  })

  def genValidTransaction(randomAmnt: Boolean = true,
                          recepientOpt: Option[PrivateKeyAccount] = None,
                          senderOpt: Option[PrivateKeyAccount] = None
                         ): Transaction = {
    val senderAcc = senderOpt.getOrElse(randomFrom(accounts))
    val senderBalance = application.consensusModule.generatingBalance(senderAcc)
    require(senderBalance > 0)
    val fee = Random.nextInt(5).toLong + 1
    if (senderBalance <= fee) {
      genValidTransaction(randomAmnt, recepientOpt, senderOpt)
    } else {
      val amt = if (randomAmnt) Math.abs(Random.nextLong() % (senderBalance - fee))
      else senderBalance - fee
      val recepient = recepientOpt.getOrElse(randomFrom(accounts))
      val tx = application.transactionModule.createPayment(senderAcc, recepient, amt, fee)
      if (application.transactionModule.blockStorage.state.isValid(tx)) tx
      else genValidTransaction(randomAmnt, recepientOpt, senderOpt)
    }
  }

  def includedTransactions(b: Block, history: History): Seq[Transaction] = {
    if (b.transactions.isEmpty) includedTransactions(history.parent(b).get, history)
    else b.transactions
  }

}
