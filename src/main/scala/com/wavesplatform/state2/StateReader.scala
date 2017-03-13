package com.wavesplatform.state2

import cats._
import cats.implicits._

import scorex.account.Account
import scorex.transaction.{Transaction, TransactionParser}

trait StateReader {
  def transactionInfo(id: ByteArray): Option[(Int, Transaction)]

  def accountPortfolio(a: Account): Portfolio

  def assetInfo(id: ByteArray): Option[AssetInfo]

  def height: Int

  def accountTransactionIds(a: Account): Seq[ByteArray]

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

  override def accountTransactionIds(a: Account): Seq[ByteArray] = Option(p.accountTransactionIds.get(a.bytes)).toSeq.map(EqByteArray)
}

class CompositeStateReader(s: StateReader, d: Diff) extends StateReader {
  override def transactionInfo(id: ByteArray): Option[(Int, Transaction)] =
    d.transactions.get(id).orElse(s.transactionInfo(id))

  override def accountPortfolio(a: Account): Portfolio =
    s.accountPortfolio(a).combine(d.portfolios.get(a).orEmpty)

  override def assetInfo(id: ByteArray): Option[AssetInfo] = d.issuedAssets.get(id).orElse(s.assetInfo(id))

  override def height: Int = s.height + d.height

  override def accountTransactionIds(a: Account): Seq[ByteArray] = {
    val newAccTxIds: Seq[ByteArray] = d.transactions.get(EqByteArray(a.bytes)).map(_._2.id).toSeq.map(EqByteArray)
    s.accountTransactionIds(a) ++ newAccTxIds
  }
}

