package com.wavesplatform.state2

import cats._
import cats.implicits._

import scorex.account.Account
import scorex.transaction.{Transaction, TransactionParser}

trait StateReader {
  def transactionInfo(id: ByteArray): Option[(Int, Transaction)]

  def accountPortfolio(a: Account): Portfolio

  def assetInfo(id: ByteArray): Option[AssetInfo]

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
}

class CompositeStateReader(s: StateReader, d: Diff) extends StateReader {
  override def transactionInfo(id: ByteArray): Option[(Int, Transaction)] =
    d.transactions.get(id).orElse(s.transactionInfo(id))

  override def accountPortfolio(a: Account): Portfolio =
    s.accountPortfolio(a).combine(d.portfolios.get(a).orEmpty)

  override def assetInfo(id: ByteArray): Option[AssetInfo] = d.issuedAssets.get(id).orElse(s.assetInfo(id))
}

