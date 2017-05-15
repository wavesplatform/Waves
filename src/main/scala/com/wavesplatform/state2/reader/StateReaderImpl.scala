package com.wavesplatform.state2.reader

import cats.implicits._
import com.wavesplatform.state2._
import scorex.account.{Account, Alias}
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.{Transaction, TransactionParser}

import scala.collection.JavaConverters._

class StateReaderImpl(p: StateStorage) extends StateReader {

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

  override def paymentTransactionIdByHash(hash: ByteArray): Option[ByteArray]
  = Option(p.paymentTransactionHashes.get(hash)).map(EqByteArray)

  override def aliasesOfAddress(a: Account): Seq[Alias] =
    p.aliasToAddress.asScala
      .collect { case (aliasName, addressBytes) if addressBytes sameElements a.bytes =>
        Alias.buildWithCurrentNetworkByte(aliasName).explicitGet()
      }.toSeq


  override def resolveAlias(a: Alias): Option[Account] =
    Option(p.aliasToAddress.get(a.name))
      .map(b => Account.fromBytes(b).explicitGet())

  override def accountPortfolios: Map[Account, Portfolio] =
    p.portfolios.asScala.map {
      case (acc, (b, (i, o), as)) => Account.fromBytes(acc).explicitGet() -> Portfolio(b, LeaseInfo(i, o), as.map {
        case (k, v) => EqByteArray(k) -> v
      })
    }.toMap

  override def isLeaseActive(leaseTx: LeaseTransaction): Boolean = p.leaseState.getOrDefault(leaseTx.id, false)

  override def activeLeases(): Seq[ByteArray] = p.leaseState
    .asScala
    .collect { case (leaseId, isActive) if isActive => EqByteArray(leaseId) }
    .toSeq

  override def lastUpdateHeight(acc: Account): Option[Int] = Option(p.lastUpdateHeight.get(acc.bytes))

  override def snapshotAtHeight(acc: Account, h: Int): Option[Snapshot] =
    Option(p.balanceSnapshots.get(StateStorage.snapshotKey(acc, h)))
      .map { case (ph, b, eb) => Snapshot(ph, b, eb) }

  override def containsTransaction(id: ByteArray): Boolean = p.transactions.containsKey(id.arr)

  override def getAssetIdByUniqueName(assetName: ByteArray): Option[ByteArray] =
    Option(p.uniqueAssets.get(assetName.arr)).map(EqByteArray)

  override def filledVolumeAndFee(orderId: ByteArray): OrderFillInfo =
    Option(p.orderFills.get(orderId.arr)).map(oi => OrderFillInfo(oi._1, oi._2)).orEmpty
}
