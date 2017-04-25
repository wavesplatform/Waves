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

    measurePersist("transactions")(txsDiff.transactions) {
      _.foreach { case (id, (h, tx, _)) =>
        p.transactions.put(id.arr, (h, tx.bytes))
      }
    }

    measurePersist("previousExchangeTxs")(blockDiff.txsDiff.previousExchangeTxs) {
      _.foreach { case (oid, txs) =>
        Option(p.exchangeTransactionsByOrder.get(oid.arr)) match {
          case Some(ll) =>
            p.exchangeTransactionsByOrder.put(oid.arr, ll ++ txs.map(_.id))
          case None =>
            p.exchangeTransactionsByOrder.put(oid.arr, txs.map(_.id))
        }
      }
    }

    measurePersist("portfolios")(txsDiff.portfolios) {
      _.foreach { case (account, portfolioDiff) =>
        val updatedPortfolio = accountPortfolio(account).combine(portfolioDiff)
        p.portfolios.put(account.bytes,
          (updatedPortfolio.balance,
            (updatedPortfolio.leaseInfo.leaseIn, updatedPortfolio.leaseInfo.leaseOut),
            updatedPortfolio.assets.map { case (k, v) => k.arr -> v }))
      }
    }


    measurePersist("assets")(txsDiff.issuedAssets) {
      _.foreach { case (id, assetInfo) =>
        val updated = (Option(p.assets.get(id.arr)) match {
          case None => Monoid[AssetInfo].empty
          case Some(existing) => AssetInfo(existing._1, existing._2)
        }).combine(assetInfo)

        p.assets.put(id.arr, (updated.isReissuable, updated.volume))
      }
    }

    measurePersist("accountTransactionIds")(blockDiff.txsDiff.accountTransactionIds) {
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

    measurePersist("paymentTransactionIdsByHashes")(blockDiff.txsDiff.paymentTransactionIdsByHashes) {
      _.foreach { case (EqByteArray(hash), EqByteArray(id)) =>
        p.paymentTransactionHashes.put(hash, id)
      }
    }

    measurePersist("effectiveBalanceSnapshots")(blockDiff.effectiveBalanceSnapshots) {
      _.foreach { ebs =>
        p.effectiveBalanceSnapshots.put((ebs.acc.bytes, ebs.height), (ebs.prevEffectiveBalance, ebs.effectiveBalance))
      }
    }
    measurePersist("aliases")(blockDiff.txsDiff.aliases) {
      _.foreach { case (alias, acc) =>
        p.aliasToAddress.put(alias.name, acc.bytes)
      }
    }

    val (effectiveNewLeases, effectiveNewCancels) = blockDiff.txsDiff.effectiveLeaseTxUpdates
    measurePersist("effectiveNewLeases")(effectiveNewLeases)(_.foreach(id => p.leaseState.put(id.arr, true)))
    measurePersist("effectiveNewCancels")(effectiveNewCancels)(_.foreach(id => p.leaseState.put(id.arr, false)))

    p.setHeight(p.getHeight + blockDiff.heightDiff)
    p.commit()
    log.debug("BlockDiff commit complete")
  }

  override def clear(): Unit = {
    p.transactions.clear()
    p.portfolios.clear()
    p.assets.clear()
    p.accountTransactionIds.clear()
    p.effectiveBalanceSnapshots.clear()
    p.paymentTransactionHashes.clear()
    p.exchangeTransactionsByOrder.clear()
    p.aliasToAddress.clear()
    p.leaseState.clear()

    p.setHeight(0)
    p.commit()
  }
}

object StateWriterImpl extends ScorexLogging {

  private def withTime[R](f: => R): (R, Long) = {
    val t0 = System.currentTimeMillis()
    val r: R = f
    val t1 = System.currentTimeMillis()
    (r, t1 - t0)
  }

  def measurePersist[F[_] <: TraversableOnce[_], A](s: String)(fa: F[A])(f: F[A] => Unit): Unit = {
    val (_, time) = withTime(f(fa))
    log.debug(s"Persisting $s(size=${fa.size}) took ${time}ms")
  }
}

