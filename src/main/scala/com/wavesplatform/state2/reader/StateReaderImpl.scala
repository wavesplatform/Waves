package com.wavesplatform.state2.reader

import cats.implicits._
import com.wavesplatform.state2._
import scorex.account.{Account, Alias}
import scorex.transaction.{Transaction, TransactionParser}

import scala.collection.JavaConverters.iterableAsScalaIterableConverter

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
    val confsOldMinimum: Seq[(Long, Long)] = Range(bockNumberThatIsConfsOld + 1, atHeight + 1)
      .flatMap { height => Option(p.effectiveBalanceSnapshots.get((acc.bytes, height))) }
    confsOldMinimum.headOption match {
      case None => accountPortfolio(acc).effectiveBalance
      case Some((oldest, _)) => Math.min(oldest, confsOldMinimum.map(_._2).min)
    }
  }

  override def paymentTransactionIdByHash(hash: ByteArray): Option[ByteArray]
  = Option(p.paymentTransactionHashes.get(hash)).map(EqByteArray)

  override def maxPaymentTransactionTimestampInPreviousBlocks(a: Account): Option[Long] =
    Option(p.maxPaymentTransactionTimestampInPreviousBlocks.get(a.bytes))

  override def aliasesOfAddress(a: Account): Seq[Alias] =
    p.aliasToAddress.entrySet().asScala
      .filter(_.getValue sameElements a.bytes)
      .map(_.getKey)
      .map(aliasStr => Alias.buildWithCurrentNetworkByte(aliasStr).right.get)
      .toSeq


  override def resolveAlias(a: Alias): Option[Account] =
    Option(p.aliasToAddress.get(a.name))
      .map(b => Account.fromBytes(b).right.get)
}
