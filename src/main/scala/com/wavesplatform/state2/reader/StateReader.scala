package com.wavesplatform.state2

import cats._
import cats.implicits._
import scorex.account.Account
import scorex.consensus.TransactionsOrdering
import scorex.transaction.{PaymentTransaction, Transaction, TransactionParser}

import scala.collection.JavaConverters.iterableAsScalaIterableConverter
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

      // This is the old-style implementation, at least 80 blocks work fine

      // The simplest and the fastest way is to add 'lastPaymentAccountTimestamp' to State and Diff

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

class StateReaderImpl(p: JavaMapStorage) extends StateReader {

  override def transactionInfo(id: ByteArray): Option[(Int, Transaction)] = Option(p.transactions.get(id.arr)).map {
    case (h, bytes) => (h, TransactionParser.parseBytes(bytes).get)
  }

  override def accountPortfolio(a: Account): Portfolio = {
    Option(p.portfolios.get(a.bytes)).map { case (b, e, as) => Portfolio(b, e, as.map { case (k, v) => EqByteArray(k) -> v }) }.orEmpty
  }

  override def assetInfo(id: ByteArray): Option[AssetInfo] = Option(p.assets.get(id.arr)).map {
    case (is, amt) => AssetInfo(is, amt)
  }

  override def height: Int = p.getHeight

  override def accountTransactionIds(a: Account): Seq[ByteArray] = {
    Option(p.accountTransactionIds.get(a.bytes))
      .map(_.asScala)
      .map(_.toSeq)
      .getOrElse(Seq.empty)
      .map(EqByteArray)
  }

  override def nonEmptyAccounts: Seq[Account] =
    p.portfolios
      .keySet()
      .asScala
      .map(b => Account.fromBytes(b).right.get)
      .toSeq

  override def effectiveBalanceAtHeightWithConfirmations(acc: Account, atHeight: Int, confs: Int): Long = {
    val bockNumberThatIsConfsOld = Math.max(1, atHeight - confs)
    val confsOldMinimum: Seq[(Long, Long)] = Range(bockNumberThatIsConfsOld + 1, atHeight + 1).flatMap { height =>

      Option(p.effectiveBalanceSnapshots.get((acc.bytes, height)))
        .map { case (prev, current) => if (height == 1) (current, current) else (prev, current) }
    }
    confsOldMinimum.headOption match {
      case None => accountPortfolio(acc).effectiveBalance
      case Some((prev, cur)) => Math.min(Math.min(prev, cur), confsOldMinimum.map(_._2).min)
    }
  }


}

class CompositeStateReader(s: StateReader, blockDiff: BlockDiff) extends StateReader {
  private val txDiff = blockDiff.txsDiff

  override def transactionInfo(id: ByteArray): Option[(Int, Transaction)] =
    txDiff.transactions.get(id).orElse(s.transactionInfo(id))

  override def accountPortfolio(a: Account): Portfolio =
    s.accountPortfolio(a).combine(txDiff.portfolios.get(a).orEmpty)

  override def assetInfo(id: ByteArray): Option[AssetInfo] =
    s.assetInfo(id).map(_.combine(txDiff.issuedAssets.get(id).orEmpty))

  override def height: Int = s.height + blockDiff.heightDiff

  override def nonEmptyAccounts: Seq[Account] =
    s.nonEmptyAccounts ++ txDiff.portfolios.keySet

  override def accountTransactionIds(a: Account): Seq[ByteArray] = ???

  override def effectiveBalanceAtHeightWithConfirmations(acc: Account, height: Int, confs: Int): Long = ???
}

