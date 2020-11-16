package com.wavesplatform.consensus

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.{ContinuationTransaction, InvokeScriptTransaction}
import com.wavesplatform.transaction.{Authorized, Transaction}

import scala.annotation.tailrec
import scala.collection.mutable

object TransactionsOrdering {
  trait WavesOrdering extends Ordering[Transaction] {
    def blockchain: Blockchain
    def utxTransactions: mutable.Map[ByteStr, Transaction]
    def isWhitelisted(t: Transaction): Boolean = false
    def txTimestampOrder(ts: Long): Long
    private def orderBy(t: Transaction): (Boolean, Double, Long, Long) = {
      val byWhiteList = !isWhitelisted(t) // false < true
      val size        = t.bytes().length
      val commonFee   = if (t.assetFee._1 != Waves) 0 else -t.assetFee._2
      val byFee       = commonFee + extraFee(t)
      val byTimestamp = txTimestampOrder(t.timestamp)

      (byWhiteList, byFee.toDouble / size.toDouble, byFee, byTimestamp)
    }

    @tailrec private def extraFee(t: Transaction): Long =
      t match {
        case i: InvokeScriptTransaction if i.assetFee._1 == Waves => -i.extraFeePerStep
        case i: ContinuationTransaction                           => extraFee(i.resolveInvoke(blockchain, utxTransactions))
        case _                                                    => 0
      }

    override def compare(first: Transaction, second: Transaction): Int = {
      import Ordering.Double.TotalOrdering
      implicitly[Ordering[(Boolean, Double, Long, Long)]].compare(orderBy(first), orderBy(second))
    }
  }

  object InBlock extends WavesOrdering {
    // sorting from network start
    override def txTimestampOrder(ts: Long): Long                   = -ts
    override def blockchain: Blockchain                             = ???
    override def utxTransactions: mutable.Map[ByteStr, Transaction] = ???
  }

  case class InUTXPool(whitelistAddresses: Set[String], blockchain: Blockchain, utxTransactions: mutable.Map[ByteStr, Transaction])
      extends WavesOrdering {
    override def isWhitelisted(t: Transaction): Boolean =
      t match {
        case _ if whitelistAddresses.isEmpty                                                            => false
        case a: Authorized if whitelistAddresses.contains(a.sender.toAddress.stringRepr)                => true
        case i: InvokeScriptTransaction if whitelistAddresses.contains(i.dAppAddressOrAlias.stringRepr) => true
        case c: ContinuationTransaction                                                                 => isWhitelisted(c.resolveInvoke(blockchain, utxTransactions))
        case _                                                                                          => false
      }
    override def txTimestampOrder(ts: Long): Long = ts
  }
}
