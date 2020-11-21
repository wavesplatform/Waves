package com.wavesplatform.consensus

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.{ContinuationTransaction, InvokeScriptTransaction}
import com.wavesplatform.transaction.{Authorized, Transaction}

import scala.annotation.tailrec
import scala.collection.mutable

case class TransactionsOrdering(whitelistAddresses: Set[String], blockchain: Blockchain, utxTransactions: mutable.Map[ByteStr, Transaction])
    extends Ordering[Transaction] {

  private def orderBy(t: Transaction): (Boolean, Double, Long, Long) = {
    val byWhiteList = !isWhitelisted(t) // false < true
    val size        = t.bytes().length
    val commonFee   = if (t.assetFee._1 != Waves) 0 else -t.assetFee._2
    val byFee       = commonFee + extraFee(t)

    (byWhiteList, byFee.toDouble / size.toDouble, byFee, t.timestamp)
  }

  @tailrec private def extraFee(t: Transaction): Long =
    t match {
      case i: InvokeScriptTransaction if i.assetFee._1 == Waves => -i.extraFeePerStep
      case c: ContinuationTransaction                           => extraFee(blockchain.resolveInvoke(c))
      case _                                                    => 0
    }

  override def compare(first: Transaction, second: Transaction): Int = {
    import Ordering.Double.TotalOrdering
    implicitly[Ordering[(Boolean, Double, Long, Long)]].compare(orderBy(first), orderBy(second))
  }

  def isWhitelisted(t: Transaction): Boolean =
    t match {
      case _ if whitelistAddresses.isEmpty                                                            => false
      case a: Authorized if whitelistAddresses.contains(a.sender.toAddress.stringRepr)                => true
      case i: InvokeScriptTransaction if whitelistAddresses.contains(i.dAppAddressOrAlias.stringRepr) => true
      case c: ContinuationTransaction                                                                 => isWhitelisted(blockchain.resolveInvoke(c))
      case _                                                                                          => false
    }
}
