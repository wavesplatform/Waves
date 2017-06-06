package com.wavesplatform.state2.reader

import java.util.concurrent.locks.ReentrantReadWriteLock

import cats.implicits._
import com.wavesplatform.state2._
import scorex.account.{Account, Alias}
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.{Transaction, TransactionParser}

import scala.collection.JavaConverters._

class StateReaderImpl(p: StateStorage, val synchronizationToken: ReentrantReadWriteLock) extends StateReader {

  val sp = Synchronized(p)

  override def transactionInfo(id: ByteStr): Option[(Int, Transaction)] = read { implicit l =>
    Option(sp().transactions.get(id)).map {
      case (h, bytes) => (h, TransactionParser.parseBytes(bytes).get)
    }
  }

  override def accountPortfolio(a: Account): Portfolio = read { implicit l =>
    Option(sp().portfolios.get(a.bytes)).map { case (b, (i, o), as) => Portfolio(b, LeaseInfo(i, o), as.map { case (k, v) => ByteStr(k) -> v }) }.orEmpty
  }

  override def assetInfo(id: ByteStr): Option[AssetInfo] = read { implicit l =>
    Option(sp().assets.get(id)).map {
      case (is, amt) => AssetInfo(is, amt)
    }
  }

  override def height: Int = read { implicit l => sp().getHeight }

  override def accountTransactionIds(a: Account): Seq[ByteStr] = read { implicit l =>
    Option(sp().accountTransactionIds.get(a.bytes))
      .map(_.toSeq)
      .getOrElse(Seq.empty)
      .map(ByteStr(_))
  }

  override def paymentTransactionIdByHash(hash: ByteStr): Option[ByteStr] = read { implicit l =>
    Option(sp().paymentTransactionHashes.get(hash))
  }

  override def aliasesOfAddress(a: Account): Seq[Alias] = read { implicit l =>
    sp().aliasToAddress.asScala
      .collect { case (aliasName, addressBytes) if addressBytes == a.bytes =>
        Alias.buildWithCurrentNetworkByte(aliasName).explicitGet()
      }.toSeq
  }

  override def resolveAlias(a: Alias): Option[Account] = read { implicit l =>
    Option(sp().aliasToAddress.get(a.name))
      .map(b => Account.fromBytes(b.arr).explicitGet())
  }

  override def accountPortfolios: Map[Account, Portfolio] = read { implicit l =>
    sp().portfolios.asScala.map {
      case (acc, (b, (i, o), as)) => Account.fromBytes(acc.arr).explicitGet() -> Portfolio(b, LeaseInfo(i, o), as.map {
        case (k, v) => ByteStr(k) -> v
      })
    }.toMap
  }

  override def isLeaseActive(leaseTx: LeaseTransaction): Boolean = read { implicit l =>
    sp().leaseState.getOrDefault(leaseTx.id, false)
  }

  override def activeLeases(): Seq[ByteStr] = read { implicit l =>
    sp().leaseState
      .asScala
      .collect { case (leaseId, isActive) if isActive => leaseId }
      .toSeq
  }

  override def lastUpdateHeight(acc: Account): Option[Int] = read { implicit l =>
    Option(sp().lastUpdateHeight.get(acc.bytes))
  }

  override def snapshotAtHeight(acc: Account, h: Int): Option[Snapshot] = read { implicit l =>
    Option(sp().balanceSnapshots.get(StateStorage.snapshotKey(acc, h)))
      .map { case (ph, b, eb) => Snapshot(ph, b, eb) }
  }

  override def containsTransaction(id: ByteStr): Boolean = read { implicit l =>
    sp().transactions.containsKey(id)
  }
  override def getAssetIdByUniqueName(assetName: ByteStr): Option[ByteStr] =read { implicit l =>
    Option(p.uniqueAssets.get(assetName))
  }

  override def filledVolumeAndFee(orderId: ByteStr): OrderFillInfo =read { implicit l =>
    Option(p.orderFills.get(orderId)).map(oi => OrderFillInfo(oi._1, oi._2)).orEmpty
  }
}
