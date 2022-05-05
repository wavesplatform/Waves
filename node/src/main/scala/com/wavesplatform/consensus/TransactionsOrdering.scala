package com.wavesplatform.consensus

import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.{Authorized, Transaction}

object TransactionsOrdering {
  trait WavesOrdering extends Ordering[Transaction] {
    def isWhitelisted(t: Transaction): Boolean = false
    def transactionSize(tx: Transaction): Int  = tx.bytes().length
    def txTimestampOrder(ts: Long): Long
    private def orderBy(t: Transaction): (Boolean, Double, Long, Long) = {
      val byWhiteList = !isWhitelisted(t) // false < true
      val size        = transactionSize(t)
      val byFee       = if (t.assetFee._1 != Waves) 0 else -t.assetFee._2
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

    override def transactionSize(tx: Transaction): Int = tx match {
      case _: ExchangeTransaction => 676 // order v3 with matcher fee in custom assets, tx V2
      case _                      => super.transactionSize(tx)
    }

    override def isWhitelisted(t: Transaction): Boolean =
      t match {
        case _ if whitelistAddresses.isEmpty                                                            => false
        case a: Authorized if whitelistAddresses.contains(a.sender.toAddress.stringRepr)                => true
        case i: InvokeScriptTransaction if whitelistAddresses.contains(i.dAppAddressOrAlias.stringRepr) => true
        case _                                                                                          => false
      }
    override def txTimestampOrder(ts: Long): Long = ts
  }
}
