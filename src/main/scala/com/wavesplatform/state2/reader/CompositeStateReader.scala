package com.wavesplatform.state2.reader

import cats.implicits._
import com.wavesplatform.state2._
import scorex.account.Account
import scorex.transaction.{PaymentTransaction, Transaction}

class CompositeStateReader(s: StateReader, blockDiff: BlockDiff) extends StateReader {
  private val txDiff = blockDiff.txsDiff

  override def transactionInfo(id: ByteArray): Option[(Int, Transaction)] =
    txDiff.transactions.get(id).orElse(s.transactionInfo(id))

  override def accountPortfolio(a: Account): Portfolio =
    s.accountPortfolio(a).combine(txDiff.portfolios.get(a).orEmpty)

  override def assetInfo(id: ByteArray): Option[AssetInfo] =
    s.assetInfo(id).map(_.combine(txDiff.issuedAssets.get(id).orEmpty))

  override def height: Int = s.height + blockDiff.heightDiff

  override def nonEmptyAccounts: Seq[Account] =
    s.nonEmptyAccounts ++ txDiff.portfolios.keySet

  override def accountTransactionIds(a: Account): Seq[ByteArray] = {
    val fromDiff = txDiff.accountTransactionIds.get(EqByteArray(a.bytes)).orEmpty
    fromDiff ++ s.accountTransactionIds(a)
  }

  override def effectiveBalanceAtHeightWithConfirmations(acc: Account, height: Int, confs: Int): Long = ???

  override def paymentTransactionIdByHash(hash: ByteArray): Option[ByteArray]
  = blockDiff.txsDiff.paymentTransactionIdsByHashes.get(hash)
    .orElse(s.paymentTransactionIdByHash(hash))

  override def maxPaymentTransactionTimestampInPreviousBlocks(a: Account): Option[Long] = {
    blockDiff.maxPaymentTransactionTimestamp.get(a)
      .orElse(s.maxPaymentTransactionTimestampInPreviousBlocks(a))
  }
}
