package com.wavesplatform.api.common

import com.wavesplatform.account.{Address, AddressOrAlias}
import com.wavesplatform.api.common
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.state.diffs.FeeValidation.FeeDetails
import com.wavesplatform.state.{Blockchain, Diff, Height, InvokeScriptResult}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.{Asset, CreateAliasTransaction, Transaction}
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import monix.reactive.Observable
import org.iq80.leveldb.DB

trait CommonTransactionsApi {
  def aliasesOfAddress(address: Address): Observable[(Height, CreateAliasTransaction)]

  def transactionsByAddress(
      subject: AddressOrAlias,
      sender: Option[Address],
      transactionTypes: Set[Byte],
      fromId: Option[ByteStr] = None
  ): Observable[(Height, Transaction)]

  def transactionById(txId: ByteStr): Option[(Int, Transaction)]

  def unconfirmedTransactions: Seq[Transaction]

  def unconfirmedTransactionById(txId: ByteStr): Option[Transaction]

  def calculateFee(tx: Transaction): Either[ValidationError, (Asset, Long, Long)]

  def broadcastTransaction(tx: Transaction): TracedResult[ValidationError, Boolean]

  def invokeScriptResults(
      subject: AddressOrAlias,
      sender: Option[Address],
      transactionTypes: Set[Byte],
      fromId: Option[ByteStr] = None
  ): Observable[(Height, Either[Transaction, (Transaction, Option[InvokeScriptResult])])]

  def invokeScriptResultById(txId: ByteStr): Option[(Height, InvokeScriptTransaction, InvokeScriptResult)]
}

object CommonTransactionsApi {
  def apply(
      maybeDiff: => Option[(Height, Diff)],
      db: DB,
      blockchain: Blockchain,
      utx: UtxPool,
      wallet: Wallet,
      publishTransaction: Transaction => TracedResult[ValidationError, Boolean]
  ): CommonTransactionsApi = new CommonTransactionsApi {
    private def resolve(subject: AddressOrAlias): Option[Address] = blockchain.resolveAlias(subject).toOption

    override def aliasesOfAddress(address: Address): Observable[(Height, CreateAliasTransaction)] = common.aliasesOfAddress(db, maybeDiff, address)

    override def transactionsByAddress(
        subject: AddressOrAlias,
        sender: Option[Address],
        transactionTypes: Set[Byte],
        fromId: Option[ByteStr] = None
    ): Observable[(Height, Transaction)] = resolve(subject).fold(Observable.empty[(Height, Transaction)]) { subjectAddress =>
      common.addressTransactions(db, maybeDiff, subjectAddress, sender, transactionTypes, fromId)
    }

    override def transactionById(transactionId: ByteStr): Option[(Int, Transaction)] =
      blockchain.transactionInfo(transactionId)

    override def unconfirmedTransactions: Seq[Transaction] = utx.all

    override def unconfirmedTransactionById(transactionId: ByteStr): Option[Transaction] =
      utx.transactionById(transactionId)

    override def calculateFee(tx: Transaction): Either[ValidationError, (Asset, Long, Long)] =
      FeeValidation
        .getMinFee(blockchain, tx)
        .map {
          case FeeDetails(asset, _, feeInAsset, feeInWaves) =>
            (asset, feeInAsset, feeInWaves)
        }

    override def broadcastTransaction(tx: Transaction): TracedResult[ValidationError, Boolean] = publishTransaction(tx)

    override def invokeScriptResults(
        subject: AddressOrAlias,
        sender: Option[Address],
        transactionTypes: Set[Byte],
        fromId: Option[ByteStr] = None
    ): Observable[(Height, Either[Transaction, (Transaction, Option[InvokeScriptResult])])] =
      resolve(subject).fold(Observable.empty[(Height, Either[Transaction, (Transaction, Option[InvokeScriptResult])])]) { subjectAddress =>
        common.invokeScriptResults(db, maybeDiff, subjectAddress, sender, transactionTypes, fromId)
      }

    override def invokeScriptResultById(txId: ByteStr): Option[(Height, InvokeScriptTransaction, InvokeScriptResult)] =
      ???
  }
}
