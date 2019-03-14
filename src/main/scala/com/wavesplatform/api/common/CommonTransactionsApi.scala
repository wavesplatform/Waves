package com.wavesplatform.api.common

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.protobuf.transaction.VanillaTransaction
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.CommonValidation
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import com.wavesplatform.transaction.{Asset, ValidationError}
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.ChannelGroup
import monix.eval.{Coeval, Task}
import monix.reactive.Observable

private[api] class CommonTransactionsApi(functionalitySettings: FunctionalitySettings,
                                         wallet: Wallet,
                                         blockchain: Blockchain,
                                         utx: UtxPool,
                                         allChannels: ChannelGroup) {

  private[this] val TransactionsBatchLimit = 100

  def transactionsByAddress(address: Address, fromId: Option[ByteStr] = None): Observable[(Int, VanillaTransaction)] = {
    val cachedAddresses = Coeval.evalOnce(concurrent.blocking((blockchain.aliasesOfAddress(address) :+ address).toSet))

    def getResponse(address: Address, limit: Int, fromId: Option[ByteStr]): Either[String, Seq[(Int, VanillaTransaction)]] = {
      blockchain.addressTransactions(address, Set.empty, limit, fromId)
    }

    def filterTxByAliases(tx: VanillaTransaction) = tx match {
      case mtt: MassTransferTransaction if mtt.sender.toAddress != address =>
        val recipients = cachedAddresses()
        mtt.copy(transfers = mtt.transfers.filter(t => recipients.contains(t.address)))

      case _ => tx
    }

    val observableTask = Task(getResponse(address, TransactionsBatchLimit, fromId)) map {
      case Right(transactions) =>
        if (transactions.isEmpty) Observable.empty
        else Observable(transactions: _*).map(ht => (ht._1, filterTxByAliases(ht._2))) ++ Observable.defer(transactionsByAddress(address, Some(transactions.last._2.id())))

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

  def broadcastTransaction(tx: VanillaTransaction): Either[ValidationError, VanillaTransaction] = {
    import com.wavesplatform.network._

    val result = for {
      r <- utx.putIfNew(tx)
      _ = if (r._1) allChannels.broadcastTx(tx, None) else ()
    } yield tx

    result
  }
}
