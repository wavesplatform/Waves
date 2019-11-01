package com.wavesplatform.api.common

import com.wavesplatform.account.Address
import com.wavesplatform.api.common
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.state.diffs.FeeValidation.FeeDetails
import com.wavesplatform.state.{Blockchain, Diff, Height}
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.{Asset, CreateAliasTransaction, Transaction}
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import org.iq80.leveldb.DB

class CommonTransactionsApi(
    db: DB,
    maybeDiff: => Option[(Height, Diff)],
    blockchain: Blockchain,
    utx: UtxPool,
    wallet: Wallet,
    publishTransaction: Transaction => TracedResult[ValidationError, Boolean]
) {
  def aliasesOfAddress(address: Address): Seq[(Height, CreateAliasTransaction)] = common.aliasesOfAddress(db, maybeDiff)(address)

  def transactionsByAddress(address: Address, transactionTypes: Set[Byte], count: Int, fromId: Option[ByteStr] = None): Seq[(Height, Transaction)] =
    common.addressTransactions(db, maybeDiff)(address, transactionTypes, count, fromId)

  def transactionById(transactionId: ByteStr): Option[(Int, Transaction)] =
    blockchain.transactionInfo(transactionId)

  def unconfirmedTransactions(): Seq[Transaction] = utx.all

  def unconfirmedTransactionById(transactionId: ByteStr): Option[Transaction] =
    utx.transactionById(transactionId)

  def calculateFee(tx: Transaction): Either[ValidationError, (Asset, Long, Long)] =
    FeeValidation
      .getMinFee(blockchain, tx)
      .map {
        case FeeDetails(asset, _, feeInAsset, feeInWaves) =>
          (asset, feeInAsset, feeInWaves)
      }

  def broadcastTransaction(tx: Transaction): TracedResult[ValidationError, Boolean] = publishTransaction(tx)
}
