package com.wavesplatform.state2.reader

import cats.implicits._
import com.wavesplatform.state2._
import com.wavesplatform.state2.diffs._
import scorex.account.{Account, Alias}
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.{Transaction, TransactionParser}

import scala.collection.JavaConverters.iterableAsScalaIterableConverter

class StateReaderImpl(p: JavaMapStorage) extends StateReader {

  override def transactionInfo(id: ByteArray): Option[(Int, Transaction)] = Option(p.transactions.get(id.arr)).map {
    case (h, bytes) => (h, TransactionParser.parseBytes(bytes).get)
  }

  override def accountPortfolio(a: Account): Portfolio = {
    Option(p.portfolios.get(a.bytes)).map { case (b, (i, o), as) => Portfolio(b, LeaseInfo(i, o), as.map { case (k, v) => EqByteArray(k) -> v }) }.orEmpty
  }

  override def assetInfo(id: ByteArray): Option[AssetInfo] = Option(p.assets.get(id.arr)).map {
    case (is, amt) => AssetInfo(is, amt)
  }

  override def height: Int = p.getHeight

  override def accountTransactionIds(a: Account): Seq[ByteArray] = {
    Option(p.accountTransactionIds.get(a.bytes))
      .map(_.toSeq)
      .getOrElse(Seq.empty)
      .map(EqByteArray)
  }

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

  override def aliasesOfAddress(a: Account): Seq[Alias] =
    p.aliasToAddress.entrySet().asScala
      .filter(_.getValue sameElements a.bytes)
      .map(_.getKey)
      .map(aliasStr => Alias.buildWithCurrentNetworkByte(aliasStr).explicitGet())
      .toSeq


  override def resolveAlias(a: Alias): Option[Account] =
    Option(p.aliasToAddress.get(a.name))
      .map(b => Account.fromBytes(b).explicitGet())

  override def findPreviousExchangeTxs(orderId: EqByteArray): Set[ExchangeTransaction] =
    Option(p.exchangeTransactionsByOrder.get(orderId.arr))
      .map(_.toSet).orEmpty
      .flatMap(id => this.findTransaction[ExchangeTransaction](id))

  override def accountPortfolios: Map[Account, Portfolio] =
    p.portfolios.entrySet().asScala
      .map { entry => entry.getKey -> entry.getValue }
      .map { case (acc, (b, (i, o), as)) => Account.fromBytes(acc).explicitGet() -> Portfolio(b, LeaseInfo(i, o), as.map { case (k, v) => EqByteArray(k) -> v }) }
      .toMap

  override def isLeaseActive(leaseTx: LeaseTransaction): Boolean = p.leaseState.getOrDefault(leaseTx.id, false)

  override def activeLeases(): Seq[ByteArray] = p.leaseState.entrySet()
    .asScala
    .filter(_.getValue)
    .map(_.getKey)
    .map(EqByteArray)
    .toSeq
}
