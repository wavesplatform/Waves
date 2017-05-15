package com.wavesplatform.state2

import cats.Monoid
import cats.implicits._
import com.wavesplatform.state2.reader.StateReaderImpl
import scorex.utils.ScorexLogging

import scala.language.higherKinds

trait StateWriter {
  def applyBlockDiff(blockDiff: BlockDiff): Unit

  def clear(): Unit
}

class StateWriterImpl(p: StateStorage) extends StateReaderImpl(p) with StateWriter with ScorexLogging {

  import StateWriterImpl._

  override def applyBlockDiff(blockDiff: BlockDiff): Unit = {
    val txsDiff = blockDiff.txsDiff

    log.debug(s"Starting persist from ${p.getHeight} to ${p.getHeight + blockDiff.heightDiff}")

    StateStorage.dirty(p) {
      measureSizeLog("transactions")(txsDiff.transactions) {
        _.foreach { case (id, (h, tx, _)) =>
          p.transactions.put(id.arr, (h, tx.bytes))
        }
      }

      measureSizeLog("orderFills")(blockDiff.txsDiff.orderFills) {
        _.foreach { case (oid, orderFillInfo) =>
          Option(p.orderFills.get(oid.arr)) match {
            case Some(ll) =>
              p.orderFills.put(oid.arr, (ll._1 + orderFillInfo.volume, ll._2 + orderFillInfo.fee))
            case None =>
              p.orderFills.put(oid.arr, (orderFillInfo.volume, orderFillInfo.fee))
          }
        }
      }

      measureSizeLog("portfolios")(txsDiff.portfolios) {
        _.foreach { case (account, portfolioDiff) =>
          val updatedPortfolio = accountPortfolio(account).combine(portfolioDiff)
          p.portfolios.put(account.bytes,
            (updatedPortfolio.balance,
              (updatedPortfolio.leaseInfo.leaseIn, updatedPortfolio.leaseInfo.leaseOut),
              updatedPortfolio.assets.map { case (k, v) => k.arr -> v }))
        }
      }


      measureSizeLog("assets")(txsDiff.issuedAssets) {
        _.foreach { case (id, assetInfo) =>
          val updated = (Option(p.assets.get(id.arr)) match {
            case None => Monoid[AssetInfo].empty
            case Some(existing) => AssetInfo(existing._1, existing._2)
          }).combine(assetInfo)

          p.assets.put(id.arr, (updated.isReissuable, updated.volume))
        }
      }

      measureSizeLog("accountTransactionIds")(blockDiff.txsDiff.accountTransactionIds) {
        _.foreach { case (acc, txIds) =>
          Option(p.accountTransactionIds.get(acc.bytes)) match {
            case Some(ll) =>
              // [h=12, h=11, h=10] ++ [h=9, ...]
              p.accountTransactionIds.put(acc.bytes, txIds.map(_.arr) ++ ll)
            case None =>
              // [h=2, h=1, h=0]
              p.accountTransactionIds.put(acc.bytes, txIds.map(_.arr))
          }
        }
      }

      measureSizeLog("paymentTransactionIdsByHashes")(blockDiff.txsDiff.paymentTransactionIdsByHashes) {
        _.foreach { case (EqByteArray(hash), EqByteArray(id)) =>
          p.paymentTransactionHashes.put(hash, id)
        }
      }

      measureSizeLog("effectiveBalanceSnapshots")(blockDiff.snapshots)(
        _.foreach { case (acc, snapshotsByHeight) =>
          snapshotsByHeight.foreach { case (h, snapshot) =>
            p.balanceSnapshots.put(StateStorage.snapshotKey(acc, h), (snapshot.prevHeight, snapshot.balance, snapshot.effectiveBalance))
          }
          p.lastUpdateHeight.put(acc.bytes, snapshotsByHeight.keys.max)
        })

      measureSizeLog("aliases")(blockDiff.txsDiff.aliases) {
        _.foreach { case (alias, acc) =>
          p.aliasToAddress.put(alias.name, acc.bytes)
        }
      }

      measureSizeLog("lease info")(blockDiff.txsDiff.leaseState)(
        _.foreach { case (id, isActive) => p.leaseState.put(id.arr, isActive) })

      measureSizeLog("uniqueAssets")(blockDiff.txsDiff.assetsWithUniqueNames) {
        _.foreach { case (name, id) =>
          p.uniqueAssets.put(name.arr, id.arr)
        }
      }

      p.setHeight(p.getHeight + blockDiff.heightDiff)
      p.commit()
    }
    log.debug("BlockDiff commit complete")
  }

  override def clear(): Unit = {
    StateStorage.dirty(p) {
      p.transactions.clear()
      p.portfolios.clear()
      p.assets.clear()
      p.accountTransactionIds.clear()
      p.balanceSnapshots.clear()
      p.paymentTransactionHashes.clear()
      p.orderFills.clear()
      p.aliasToAddress.clear()
      p.leaseState.clear()
      p.lastUpdateHeight.clear()
      p.uniqueAssets.clear()
      p.setHeight(0)
      p.commit()
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

