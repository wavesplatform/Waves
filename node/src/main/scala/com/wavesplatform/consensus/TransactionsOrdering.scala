package com.wavesplatform.consensus

import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.{ContinuationTransaction, InvokeScriptTransaction}
import com.wavesplatform.transaction.{Authorized, Transaction}

case class TransactionsOrdering(
    whitelistAddresses: Set[String],
    resolveInvoke: ContinuationTransaction => Option[InvokeScriptTransaction]
) extends Ordering[Transaction] {

  private def orderBy(tx: Transaction): (Boolean, Double, Long, Long) = {
    val commonFee = if (tx.assetFee._1 != Waves) 0 else -tx.assetFee._2
    val size      = tx.bytes().length

    val resolvedTx  = maybeContinuation(tx)
    val byWhiteList = !isWhitelisted(resolvedTx) // false < true
    val byFee       = commonFee + extraFee(resolvedTx)

    (byWhiteList, byFee.toDouble / size.toDouble, byFee, resolvedTx.timestamp)
  }

  private def extraFee(t: Transaction): Long =
    t match {
      case i: InvokeScriptTransaction if i.assetFee._1 == Waves => -i.extraFeePerStep
      case _                                                    => 0
    }

  private def maybeContinuation(t: Transaction): Transaction =
    t match {
      case c: ContinuationTransaction => resolveInvoke(c).getOrElse(c)
      case _                          => t
    }

  override def compare(first: Transaction, second: Transaction): Int = {
    import Ordering.Double.TotalOrdering
    implicitly[Ordering[(Boolean, Double, Long, Long)]].compare(orderBy(first), orderBy(second))
  }

  def isWhitelisted(t: Transaction): Boolean =
    maybeContinuation(t) match {
      case _ if whitelistAddresses.isEmpty                                                            => false
      case a: Authorized if whitelistAddresses.contains(a.sender.toAddress.stringRepr)                => true
      case i: InvokeScriptTransaction if whitelistAddresses.contains(i.dAppAddressOrAlias.stringRepr) => true
      case _                                                                                          => false
    }
}
