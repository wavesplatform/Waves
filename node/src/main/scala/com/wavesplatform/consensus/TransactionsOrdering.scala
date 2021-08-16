package com.wavesplatform.consensus

import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.{Authorized, Transaction}

object TransactionsOrdering {
  trait WavesOrdering extends Ordering[Transaction] {
    def isWhitelisted(t: Transaction): Boolean = false
    def txTimestampOrder(ts: Long): Long
    private def orderBy(t: Transaction): (Boolean, Double, Long, Long) = {
      val byWhiteList = !isWhitelisted(t) // false < true
      val size        = t.bytes().length
      val byFee       = if (t.feeAssetId != Waves) 0 else -t.fee
      val byTimestamp = txTimestampOrder(t.timestamp)

      (byWhiteList, byFee.toDouble / size.toDouble, byFee, byTimestamp)
    }
    override def compare(first: Transaction, second: Transaction): Int = {
      import Ordering.Double.TotalOrdering
      implicitly[Ordering[(Boolean, Double, Long, Long)]].compare(orderBy(first), orderBy(second))
    }
  }

  object InBlock extends WavesOrdering {
    // sorting from network start
    override def txTimestampOrder(ts: Long): Long = -ts
  }

  case class InUTXPool(whitelistAddresses: Set[String]) extends WavesOrdering {
    override def isWhitelisted(t: Transaction): Boolean =
      t match {
        case _ if whitelistAddresses.isEmpty                                                            => false
        case a: Authorized if whitelistAddresses.contains(a.sender.toAddress.toString)                => true
        case i: InvokeScriptTransaction if whitelistAddresses.contains(i.dApp.toString) => true
        case _                                                                                          => false
      }
    override def txTimestampOrder(ts: Long): Long = ts
  }
}
