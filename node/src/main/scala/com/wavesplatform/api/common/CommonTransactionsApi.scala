package com.wavesplatform.api.common

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.transaction.VanillaTransaction
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.CommonValidation
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import monix.eval.Task
import monix.reactive.Observable

private[api] class CommonTransactionsApi(functionalitySettings: FunctionalitySettings,
                                         wallet: Wallet,
                                         blockchain: Blockchain,
                                         utx: UtxPool,
                                         broadcast: VanillaTransaction => Unit) {

  private[this] val TransactionsBatchLimit = 100

  def transactionsByAddress(address: Address, fromId: Option[ByteStr] = None): Observable[(Int, VanillaTransaction)] = {
    def getTransactionsList(address: Address, limit: Int, fromId: Option[ByteStr]): Either[String, Seq[(Int, VanillaTransaction)]] =
      concurrent.blocking {
        blockchain.addressTransactions(address, Set.empty, limit, fromId)
      }

    val observableTask = Task(getTransactionsList(address, TransactionsBatchLimit, fromId)) map {
      case Right(transactions) =>
        if (transactions.isEmpty) Observable.empty
        else Observable(transactions: _*) ++ Observable.defer(transactionsByAddress(address, Some(transactions.last._2.id())))

      case Left(err) =>
        Observable.raiseError(new IllegalArgumentException(err))
    }

    Observable.fromTask(observableTask).flatten
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
      _ = if (shouldBroadcast) broadcast(tx) else ()
    } yield tx

    result
  }
}
