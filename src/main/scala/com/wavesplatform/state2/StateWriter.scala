package com.wavesplatform.state2

import java.util.concurrent.locks.ReentrantReadWriteLock

import cats.Monoid
import cats.implicits._
import com.wavesplatform.metrics.Instrumented
import com.wavesplatform.state2.reader.StateReaderImpl
import scorex.transaction.PaymentTransaction
import scorex.transaction.assets.TransferTransaction
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.utils.ScorexLogging

trait StateWriter {
  def applyBlockDiff(blockDiff: BlockDiff): Unit

  def clear(): Unit
}

class StateWriterImpl(p: StateStorage, storeTransactions: Boolean, synchronizationToken: ReentrantReadWriteLock)
  extends StateReaderImpl(p, synchronizationToken) with StateWriter with AutoCloseable with ScorexLogging with Instrumented {

  import StateStorage._

  override def close(): Unit = p.close()

  override def applyBlockDiff(blockDiff: BlockDiff): Unit = write { implicit l =>
    val txsDiff = blockDiff.txsDiff

    val oldHeight = sp().getHeight
    val newHeight = oldHeight + blockDiff.heightDiff
    log.debug(s"Starting persist from $oldHeight to $newHeight")

    measureSizeLog("transactions")(txsDiff.transactions) { txs =>
      txs.toSeq.foreach {
        case (id, (h, tx@(_: TransferTransaction | _: ExchangeTransaction | _: PaymentTransaction), _)) =>
          sp().transactions.put(id, (h, if (storeTransactions) tx.bytes() else Array.emptyByteArray))
        case (id, (h, tx, _)) => sp().transactions.put(id, (h, tx.bytes()))
      }
    }

    measureSizeLog("orderFills")(blockDiff.txsDiff.orderFills) { ofs =>
      ofs.toSeq.foreach {
        case (oid, orderFillInfo) =>
          Option(sp().orderFills.get(oid)) match {
            case Some(ll) =>
              sp().orderFills.put(oid, (ll._1 + orderFillInfo.volume, ll._2 + orderFillInfo.fee))
            case None =>
              sp().orderFills.put(oid, (orderFillInfo.volume, orderFillInfo.fee))
          }
      }
    }

    measureSizeLog("portfolios")(txsDiff.portfolios) {
      _.foreach { case (account, portfolioDiff) =>
        val updatedPortfolio = this.partialPortfolio(account, portfolioDiff.assets.keySet).combine(portfolioDiff)
        sp().wavesBalance.put(account.bytes, (updatedPortfolio.balance, updatedPortfolio.leaseInfo.leaseIn, updatedPortfolio.leaseInfo.leaseOut))
        updatedPortfolio.assets.foreach { case (asset, amt) =>
          sp().assetBalance.put(account.bytes, asset, amt)
        }
      }
    }

    measureSizeLog("assets")(txsDiff.issuedAssets) {
      _.foreach { case (id, assetInfo) =>
        val updated = (Option(sp().assets.get(id)) match {
          case None => Monoid[AssetInfo].empty
          case Some(existing) => AssetInfo(existing._1, existing._2)
        }).combine(assetInfo)

        sp().assets.put(id, (updated.isReissuable, updated.volume))
      }
    }

    if (storeTransactions) measureSizeLog("accountTransactionIds")(blockDiff.txsDiff.accountTransactionIds) {
      _.foreach { case (acc, txIds) =>
        val startIdxShift = sp().accountTransactionsLengths.getOrDefault(acc.bytes, 0)
        txIds.reverse.foldLeft(startIdxShift) { case (shift, txId) =>
          sp().accountTransactionIds.put(accountIndexKey(acc, shift), txId)
          shift + 1
        }
        sp().accountTransactionsLengths.put(acc.bytes, startIdxShift + txIds.length)
      }
    }

    measureSizeLog("paymentTransactionIdsByHashes")(blockDiff.txsDiff.paymentTransactionIdsByHashes) {
      _.foreach { case (hash, id) =>
        sp().paymentTransactionHashes.put(hash, id)
      }
    }

    measureSizeLog("effectiveBalanceSnapshots")(blockDiff.snapshots)(
      _.foreach { case (acc, snapshotsByHeight) =>
        snapshotsByHeight.foreach { case (h, snapshot) =>
          sp().balanceSnapshots.put(accountIndexKey(acc, h), (snapshot.prevHeight, snapshot.balance, snapshot.effectiveBalance))
        }
        sp().lastBalanceSnapshotHeight.put(acc.bytes, snapshotsByHeight.keys.max)
      })

    measureSizeLog("aliases")(blockDiff.txsDiff.aliases) {
      _.foreach { case (alias, acc) =>
        sp().aliasToAddress.put(alias.name, acc.bytes)
      }
    }

    measureSizeLog("lease info")(blockDiff.txsDiff.leaseState)(
      _.foreach { case (id, isActive) => sp().leaseState.put(id, isActive) })

    sp().setHeight(newHeight)
    val nextChunkOfBlocks = !sameQuotient(newHeight, oldHeight, 1000)
    sp().commit(nextChunkOfBlocks)
    log.info(s"BlockDiff commit complete. Persisted height = $newHeight")
  }

  override def clear(): Unit = write { implicit l =>
    sp().transactions.clear()
    sp().wavesBalance.clear()
    sp().assetBalance.clear()
    sp().assets.clear()
    sp().accountTransactionIds.clear()
    sp().accountTransactionsLengths.clear()
    sp().balanceSnapshots.clear()
    sp().paymentTransactionHashes.clear()
    sp().orderFills.clear()
    sp().aliasToAddress.clear()
    sp().leaseState.clear()
    sp().lastBalanceSnapshotHeight.clear()
    sp().setHeight(0)
    sp().commit(compact = true)
  }
}
