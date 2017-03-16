package com.wavesplatform.state2

import cats._
import cats.implicits._
import scorex.account.Account
import scorex.transaction.{PaymentTransaction, Transaction, TransactionParser}

import scala.collection.JavaConverters.iterableAsScalaIterableConverter

trait StateReader {
  def transactionInfo(id: ByteArray): Option[(Int, Transaction)]

  def accountPortfolio(a: Account): Portfolio

  def assetInfo(id: ByteArray): Option[AssetInfo]

  def height: Int

  def accountTransactionIds(a: Account): Seq[ByteArray]

  def nonEmptyAccounts: Seq[Account]

}

object StateReader {

  implicit class StateReaderExt(r: StateReader) {
    def assetDistribution(assetId: ByteArray): Map[Account, Long] =
      r.nonEmptyAccounts
        .flatMap(acc => r.accountPortfolio(acc).assets.get(assetId).map(acc -> _))
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

      r.accountTransactionIds(account).toList
        .flatMap(id => r.transactionInfo(id))
        .filter { case (h, t) => t.isInstanceOf[PaymentTransaction] }
        .map { case (h, t) => (h, t.asInstanceOf[PaymentTransaction]) }
        .filter { case (h, t) => t.sender.bytes sameElements account.bytes }
        .groupBy(_._1)
        .filter { case (h, ptxs) => ptxs.nonEmpty }
        .map { case (h, st) => (h, st.map(_._2)) }
        .toList
        .sortBy(-_._1)
        .collectFirst { case t => t }
        .map(_._2.sortBy(-_.timestamp).head)

    }
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
}

class CompositeStateReader(s: StateReader, blockDiff: BlockDiff) extends StateReader {
  private val txDiff = blockDiff.txsDiff

  override def transactionInfo(id: ByteArray): Option[(Int, Transaction)] =
    txDiff.transactions.get(id).orElse(s.transactionInfo(id))

  override def accountPortfolio(a: Account): Portfolio =
    s.accountPortfolio(a).combine(txDiff.portfolios.get(a).orEmpty)

  override def assetInfo(id: ByteArray): Option[AssetInfo] = txDiff.issuedAssets.get(id).orElse(s.assetInfo(id))

  override def height: Int = s.height + blockDiff.heightDiff

  override def accountTransactionIds(a: Account): Seq[ByteArray] = {
    //    val newAccTxIds: Seq[ByteArray] = txDiff.transactions.get(EqByteArray(a.bytes)).map(_._2.id).toSeq.map(EqByteArray)
    //    s.accountTransactionIds(a) ++ newAccTxIds
    ???
  }

  override def nonEmptyAccounts: Seq[Account] =
    s.nonEmptyAccounts ++ txDiff.portfolios.keySet
}

