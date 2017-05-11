package com.wavesplatform.state2

import java.util.concurrent.locks.ReentrantReadWriteLock

import cats.Monoid
import cats.implicits._
import com.wavesplatform.state2.reader.StateReaderImpl
import scorex.utils.ScorexLogging

import scala.language.higherKinds

trait StateWriter {
  def applyBlockDiff(blockDiff: BlockDiff): Unit

  def clear(): Unit
}

class StateWriterImpl(p: StateStorage, synchronizationToken: ReentrantReadWriteLock)
  extends StateReaderImpl(p, synchronizationToken) with StateWriter with ScorexLogging {

  import StateWriterImpl._

  override def applyBlockDiff(blockDiff: BlockDiff): Unit = write { implicit l =>
    val txsDiff = blockDiff.txsDiff

    log.debug(s"Starting persist from ${sp().getHeight} to ${sp().getHeight + blockDiff.heightDiff}")

    StateStorage.dirty(p) {
      measureSizeLog("transactions")(txsDiff.transactions) {
        _.foreach { case (id, (h, tx, _)) =>
          sp().transactions.put(id.arr, (h, tx.bytes))
        }
      }

      measureSizeLog("previousExchangeTxs")(blockDiff.txsDiff.previousExchangeTxs) {
        _.foreach { case (oid, txs) =>
          Option(sp().exchangeTransactionsByOrder.get(oid.arr)) match {
            case Some(ll) =>
              sp().exchangeTransactionsByOrder.put(oid.arr, ll ++ txs.map(_.id))
            case None =>
              sp().exchangeTransactionsByOrder.put(oid.arr, txs.map(_.id))
          }
        }
      }

      measureSizeLog("portfolios")(txsDiff.portfolios) {
        _.foreach { case (account, portfolioDiff) =>
          val updatedPortfolio = accountPortfolio(account).combine(portfolioDiff)
          sp().portfolios.put(account.bytes,
            (updatedPortfolio.balance,
              (updatedPortfolio.leaseInfo.leaseIn, updatedPortfolio.leaseInfo.leaseOut),
              updatedPortfolio.assets.map { case (k, v) => k.arr -> v }))
        }
      }


      measureSizeLog("assets")(txsDiff.issuedAssets) {
        _.foreach { case (id, assetInfo) =>
          val updated = (Option(sp().assets.get(id.arr)) match {
            case None => Monoid[AssetInfo].empty
            case Some(existing) => AssetInfo(existing._1, existing._2)
          }).combine(assetInfo)

          sp().assets.put(id.arr, (updated.isReissuable, updated.volume))
        }
      }

      measureSizeLog("accountTransactionIds")(blockDiff.txsDiff.accountTransactionIds) {
        _.foreach { case (acc, txIds) =>
          Option(sp().accountTransactionIds.get(acc.bytes)) match {
            case Some(ll) =>
              // [h=12, h=11, h=10] ++ [h=9, ...]
              sp().accountTransactionIds.put(acc.bytes, txIds.map(_.arr) ++ ll)
            case None =>
              // [h=2, h=1, h=0]
              sp().accountTransactionIds.put(acc.bytes, txIds.map(_.arr))
          }
        }
      }

      measureSizeLog("paymentTransactionIdsByHashes")(blockDiff.txsDiff.paymentTransactionIdsByHashes) {
        _.foreach { case (EqByteArray(hash), EqByteArray(id)) =>
          sp().paymentTransactionHashes.put(hash, id)
        }
      }

      measureSizeLog("effectiveBalanceSnapshots")(blockDiff.snapshots)(
        _.foreach { case (acc, snapshotsByHeight) =>
          snapshotsByHeight.foreach { case (h, snapshot) =>
            sp().balanceSnapshots.put(StateStorage.snapshotKey(acc, h), (snapshot.prevHeight, snapshot.balance, snapshot.effectiveBalance))
          }
          sp().lastUpdateHeight.put(acc.bytes, snapshotsByHeight.keys.max)
        })

      measureSizeLog("aliases")(blockDiff.txsDiff.aliases) {
        _.foreach { case (alias, acc) =>
          sp().aliasToAddress.put(alias.name, acc.bytes)
        }
      }

      measureSizeLog("lease info")(blockDiff.txsDiff.leaseState)(
        _.foreach { case (id, isActive) => sp().leaseState.put(id.arr, isActive) })

      measureSizeLog("uniqueAssets")(blockDiff.txsDiff.assetsWithUniqueNames) {
        _.foreach { case (name, id) =>
          p.uniqueAssets.put(name.arr, id.arr)
        }
      }

      sp().setHeight(sp().getHeight + blockDiff.heightDiff)
      sp().commit()
    }
    log.debug("BlockDiff commit complete")
  }

  override def clear(): Unit = write { implicit l =>
    StateStorage.dirty(p) {
      sp().transactions.clear()
      sp().portfolios.clear()
      sp().assets.clear()
      sp().accountTransactionIds.clear()
      sp().balanceSnapshots.clear()
      sp().paymentTransactionHashes.clear()
      sp().exchangeTransactionsByOrder.clear()
      sp().aliasToAddress.clear()
      sp().leaseState.clear()
      sp().lastUpdateHeight.clear()
      sp().uniqueAssets.clear()
      sp().setHeight(0)
      sp().commit()

    }
  }
}

object StateWriterImpl extends ScorexLogging {

  private def withTime[R](f: => R): (R, Long) = {
    val t0 = System.currentTimeMillis()
    val r: R = f
    val t1 = System.currentTimeMillis()
    (r, t1 - t0)
  }

  def measureSizeLog[F[_] <: TraversableOnce[_], A, R](s: String)(fa: => F[A])(f: F[A] => R): R = {
    val (r, time) = withTime(f(fa))
    log.debug(s"processing of ${fa.size} $s took ${time}ms")
    r
  }

  def measureLog[R](s: String)(f: => R): R = {
    val (r, time) = withTime(f)
    log.debug(s"$s took ${time}ms")
    r
  }
}

