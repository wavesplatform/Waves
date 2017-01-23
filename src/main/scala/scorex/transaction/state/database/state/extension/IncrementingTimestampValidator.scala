package scorex.transaction.state.database.state.extension

import scorex.account.Account
import scorex.settings.WavesHardForkParameters
import scorex.transaction.state.database.state._
import scorex.transaction.state.database.state.storage.StateStorageI
import scorex.transaction.{PaymentTransaction, Transaction}

class IncrementingTimestampValidator(settings: WavesHardForkParameters, storage: StateStorageI) extends StateExtension {

  override def isValid(transaction: Transaction, height: Int): Boolean = transaction match {
    case tx: PaymentTransaction =>
      tx.timestamp < settings.allowInvalidPaymentTransactionsByTimestamp || isTimestampCorrect(tx)
    case _ => true
  }

  private def isTimestampCorrect(tx: PaymentTransaction): Boolean = {
    lastAccountPaymentTransaction(tx.sender) match {
      case Some(lastTransaction) => lastTransaction.timestamp < tx.timestamp
      case None => true
    }
  }


  def invalidatePaymentTransactionsByTimestamp(transactions: Seq[Transaction]): Seq[Transaction] = {
    val paymentTransactions = transactions.filter(_.isInstanceOf[PaymentTransaction])
      .map(_.asInstanceOf[PaymentTransaction])

    val initialSelection: Map[String, (List[Transaction], Long)] = Map(paymentTransactions.map { payment =>
      val address = payment.sender.address
      val stateTimestamp = lastAccountPaymentTransaction(payment.sender) match {
        case Some(lastTransaction) => lastTransaction.timestamp
        case _ => 0
      }
      address -> (List[Transaction](), stateTimestamp)
    }: _*)

    val orderedTransaction = paymentTransactions.sortBy(_.timestamp)
    val selection: Map[String, (List[Transaction], Long)] = orderedTransaction.foldLeft(initialSelection) { (s, t) =>
      val address = t.sender.address
      val tuple = s(address)
      if (t.timestamp > tuple._2) {
        s.updated(address, (tuple._1, t.timestamp))
      } else {
        s.updated(address, (tuple._1 :+ t, tuple._2))
      }
    }

    selection.foldLeft(List[Transaction]()) { (l, s) => l ++ s._2._1 }
  }

  def lastAccountPaymentTransaction(account: Account): Option[PaymentTransaction] = {
    def loop(h: Int, address: Address): Option[PaymentTransaction] = {
      storage.getAccountChanges(address, h) match {
        case Some(row) =>
          val accountTransactions = row.reason.flatMap(id => storage.getTransaction(id))
            .filter(_.isInstanceOf[PaymentTransaction])
            .map(_.asInstanceOf[PaymentTransaction])
            .filter(_.sender.address == address)
          if (accountTransactions.nonEmpty) Some(accountTransactions.maxBy(_.timestamp))
          else loop(row.lastRowHeight, address)
        case _ => None
      }
    }

    storage.getLastStates(account.address) match {
      case Some(height) => loop(height, account.address)
      case None => None
    }
  }


  override def process(tx: Transaction, blockTs: Long, height: Int): Unit = {}
}