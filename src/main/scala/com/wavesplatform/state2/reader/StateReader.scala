package com.wavesplatform.state2.reader

import com.wavesplatform.state2._
import scorex.account.Account
import scorex.consensus.TransactionsOrdering
import scorex.transaction.{PaymentTransaction, Transaction}

import scala.reflect.ClassTag

trait StateReader {


  def transactionInfo(id: ByteArray): Option[(Int, Transaction)]

  def accountPortfolio(a: Account): Portfolio

  def assetInfo(id: ByteArray): Option[AssetInfo]

  def height: Int

  def accountTransactionIds(a: Account): Seq[ByteArray]

  def nonEmptyAccounts: Seq[Account]

  def effectiveBalanceAtHeightWithConfirmations(acc: Account, height: Int, confs: Int): Long

}

object StateReader {

  implicit class StateReaderExt(s: StateReader) {
    def assetDistribution(assetId: ByteArray): Map[Account, Long] =
      s.nonEmptyAccounts
        .flatMap(acc => s.accountPortfolio(acc).assets.get(assetId).map(acc -> _))
        .toMap

    def lastAccountPaymentTransaction(account: Account): Option[PaymentTransaction] = {

      // #1 The old state returns tx with the greatest timestamp of the most recent block
      // which contains outgoing payment transactions from the account.

      // #2 The 'correct' implementation would be to return the most recent Payment tx of
      // the account as the transaction application advances:

      //      r.accountTransactionIds(account).toStream
      //        .flatMap(id => r.transactionInfo(id))
      //        .filter { case (id, t) => t.isInstanceOf[PaymentTransaction] }
      //        .map { case (id, t) => t.asInstanceOf[PaymentTransaction] }
      //        .filter(t => t.sender.bytes sameElements account.bytes)
      //        .collectFirst { case t => t }

      // Until we are not sure we can change the logic from #1 to #2,
      // the temporary fix is to return the most recent outgoing Payment tx by timestamp.
      // This doesn't work for block #71

      //      r.accountTransactionIds(account).toStream
      //        .flatMap(id => r.transactionInfo(id))
      //        .filter { case (id, t) => t.isInstanceOf[PaymentTransaction] }
      //        .map { case (id, t) => t.asInstanceOf[PaymentTransaction] }
      //        .toList
      //        .filter(t => t.sender.bytes sameElements account.bytes)
      //        .sortBy(-_.timestamp)
      //        .collectFirst { case t => t }

      // #3 This is the old-style implementation, at least 80 blocks work fine

      s.accountTransactionIds(account).toList
        .flatMap(id => s.transactionInfo(id))
        .filter { case (h, t) => t.isInstanceOf[PaymentTransaction] }
        .map { case (h, t) => (h, t.asInstanceOf[PaymentTransaction]) }
        .filter { case (h, t) => t.sender.bytes sameElements account.bytes }
        .groupBy(_._1)
        .filter { case (h, ptxs) => ptxs.nonEmpty }
        .map { case (h, st) => (h, st.map(_._2)) }
        .toList
        .sortBy(-_._1)
        .collectFirst { case t => t }
        .map(_._2.sorted(TransactionsOrdering.InBlock).head)

      // #4 The simplest and the fastest way is to add 'lastPaymentAccountTimestamp' to State and Diff

    }

    def findTransaction[T <: Transaction](signature: Array[Byte])(implicit ct: ClassTag[T]): Option[T]
    = s.transactionInfo(EqByteArray(signature)).map(_._2)
      .flatMap(tx => {
        if (ct.runtimeClass.isAssignableFrom(tx.getClass))
          Some(tx.asInstanceOf[T])
        else None
      })
  }

}

