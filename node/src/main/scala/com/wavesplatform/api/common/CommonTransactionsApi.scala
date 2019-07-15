package com.wavesplatform.api.common

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.transaction.VanillaTransaction
import com.wavesplatform.state.diffs.CommonValidation
import com.wavesplatform.state.{Blockchain, Height}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import monix.reactive.Observable

private[api] class CommonTransactionsApi(blockchain: Blockchain, utx: UtxPool, wallet: Wallet, broadcast: (VanillaTransaction, Boolean) => Unit) {

  def transactionsByAddress(address: Address, fromId: Option[ByteStr] = None): Observable[(Height, VanillaTransaction)] = {
    val seq = blockchain.addressTransactions(address, Set.empty, Int.MaxValue, fromId) // FIXME fix tests instead
    Observable.fromIterable(seq.getOrElse(Nil))
  }

  def transactionById(transactionId: ByteStr): Option[(Int, VanillaTransaction)] = {
    blockchain.transactionInfo(transactionId)
  }

  def unconfirmedTransactions(): Seq[VanillaTransaction] = {
    utx.all
  }

  def unconfirmedTransactionById(transactionId: ByteStr): Option[VanillaTransaction] = {
    utx.transactionById(transactionId)
  }

  def calculateFee(tx: VanillaTransaction): Either[ValidationError, (Asset, Long, Long)] = {
    CommonValidation.getMinFee(blockchain, tx)
  }

  def broadcastTransaction(tx: VanillaTransaction): TracedResult[ValidationError, VanillaTransaction] = {
    val result = for {
      isNew <- utx.putIfNew(tx)
      _ = broadcast(tx, isNew)
    } yield tx

    result
  }
}
