package com.wavesplatform.state2.reader

import cats.implicits._
import com.wavesplatform.state2._
import scorex.account.Account
import scorex.transaction.Transaction

class CompositeStateReader(inner: StateReader, blockDiff: BlockDiff) extends StateReader {
  private val txDiff = blockDiff.txsDiff

  override def transactionInfo(id: ByteArray): Option[(Int, Transaction)] =
    txDiff.transactions.get(id).orElse(inner.transactionInfo(id))

  override def accountPortfolio(a: Account): Portfolio =
    inner.accountPortfolio(a).combine(txDiff.portfolios.get(a).orEmpty)

  override def assetInfo(id: ByteArray): Option[AssetInfo] =
    inner.assetInfo(id).map(_.combine(txDiff.issuedAssets.get(id).orEmpty))

  override def height: Int = inner.height + blockDiff.heightDiff

  override def nonEmptyAccounts: Seq[Account] =
    inner.nonEmptyAccounts ++ txDiff.portfolios.keySet

  override def accountTransactionIds(a: Account): Seq[ByteArray] = {
    val fromDiff = txDiff.accountTransactionIds.get(EqByteArray(a.bytes)).orEmpty
    fromDiff ++ inner.accountTransactionIds(a)
  }

  override def effectiveBalanceAtHeightWithConfirmations(acc: Account, height: Int, confs: Int): Long = {

    val localEffectiveBalanceSnapshotsOfAccount = blockDiff.effectiveBalanceSnapshots.filter(_.acc == acc)

    if (acc.address == "3P8JdJGYc7vaLu4UXUZc1iRLdzrkGtdCyJM") {
      println(s"acc: $acc")
      println(s"height: $height")
      println(s"blockDiff.heightDiff: ${blockDiff.heightDiff}")
      println(s"confs: $confs")
      println(s"confs <= blockDiff.heightDiff: ${confs <= blockDiff.heightDiff}")
      println(s"localEffectiveBalanceSnapshotsOfAccount = $localEffectiveBalanceSnapshotsOfAccount")
    }

    lazy val storedEffectiveBalance = inner.effectiveBalanceAtHeightWithConfirmations(acc, height - blockDiff.heightDiff, confs - blockDiff.heightDiff)

    if (localEffectiveBalanceSnapshotsOfAccount.isEmpty) {
      storedEffectiveBalance
    } else {
      lazy val needToRequestOlderEntities = confs <= blockDiff.heightDiff
      lazy val canRequestOlderEntities = inner.height >= 1

      val localMinEffectiveBalance = localEffectiveBalanceSnapshotsOfAccount.map(_.effectiveBalance).min
      if (needToRequestOlderEntities) {
        if (canRequestOlderEntities) {
          Math.min(localMinEffectiveBalance, storedEffectiveBalance)
        } else {
          Math.min(localMinEffectiveBalance, localEffectiveBalanceSnapshotsOfAccount.last.prevEffectiveBalance)
        }
      } else {
        localMinEffectiveBalance
      }
    }
  }

  override def paymentTransactionIdByHash(hash: ByteArray): Option[ByteArray]
  = blockDiff.txsDiff.paymentTransactionIdsByHashes.get(hash)
    .orElse(inner.paymentTransactionIdByHash(hash))

  override def maxPaymentTransactionTimestampInPreviousBlocks(a: Account): Option[Long] = {
    blockDiff.maxPaymentTransactionTimestamp.get(a)
      .orElse(inner.maxPaymentTransactionTimestampInPreviousBlocks(a))
  }
}
