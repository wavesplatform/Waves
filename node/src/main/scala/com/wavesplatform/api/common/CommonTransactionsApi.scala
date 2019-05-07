package com.wavesplatform.api.common

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.transaction.VanillaTransaction
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.diffs.CommonValidation
import com.wavesplatform.transaction.Asset
import com.wavesplatform.state.{Blockchain, Height}
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import monix.reactive.Observable

private[api] class CommonTransactionsApi(functionalitySettings: FunctionalitySettings,
                                         wallet: Wallet,
                                         blockchain: Blockchain,
                                         utx: UtxPool,
                                         broadcast: VanillaTransaction => Unit) {

  def transactionsByAddress(address: Address, fromId: Option[ByteStr] = None): Observable[(Height, VanillaTransaction)] = {
    val iterator = blockchain.addressTransactions(address, Set.empty, fromId)
    Observable.fromIterator(iterator, () => iterator.close())
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
    CommonValidation.getMinFee(blockchain, functionalitySettings, blockchain.height, tx)
  }

  def broadcastTransaction(tx: VanillaTransaction): TracedResult[ValidationError, VanillaTransaction] = {
    val result = for {
      shouldBroadcast <- utx.putIfNew(tx)
      _ = broadcast(tx)
    } yield tx

    result
  }
}
