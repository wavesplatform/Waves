package com.wavesplatform.api.common

import scala.concurrent.Future

import com.wavesplatform.account.{Address, AddressOrAlias}
import com.wavesplatform.api.{common, BlockMeta}
import com.wavesplatform.block
import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.TransactionProof
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.{Blockchain, Diff, Height, InvokeScriptResult}
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.state.diffs.FeeValidation.FeeDetails
import com.wavesplatform.state.diffs.invoke.InvokeScriptTransactionDiff
import com.wavesplatform.transaction.{Asset, CreateAliasTransaction, EthereumTransaction, Transaction}
import com.wavesplatform.transaction.TransactionType.TransactionType
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import monix.reactive.Observable
import org.iq80.leveldb.DB

trait CommonTransactionsApi {
  import CommonTransactionsApi._

  def aliasesOfAddress(address: Address): Observable[(Height, CreateAliasTransaction)]

  def transactionById(txId: ByteStr): Option[TransactionMeta]

  def unconfirmedTransactions: Seq[Transaction]

  def unconfirmedTransactionById(txId: ByteStr): Option[Transaction]

  def calculateFee(tx: Transaction): Either[ValidationError, (Asset, Long, Long)]

  def broadcastTransaction(tx: Transaction): Future[TracedResult[ValidationError, Boolean]]

  def transactionsByAddress(
      subject: Address,
      sender: Option[Address],
      transactionTypes: Set[TransactionType],
      fromId: Option[ByteStr] = None
  ): Observable[TransactionMeta]

  def transactionProofs(transactionIds: List[ByteStr]): List[TransactionProof]
}

object CommonTransactionsApi {
  sealed trait TransactionMeta {
    def height: Height
    def transaction: Transaction
    def succeeded: Boolean
  }

  object TransactionMeta {
    final case class Default(height: Height, transaction: Transaction, succeeded: Boolean) extends TransactionMeta

    final case class Invoke(height: Height, transaction: Transaction, succeeded: Boolean, invokeScriptResult: Option[InvokeScriptResult])
        extends TransactionMeta {
      def dApp: AddressOrAlias = transaction match {
        case EthereumTransaction(EthereumTransaction.Invocation(dApp, _), _, _, _) => dApp
        case ist: InvokeScriptTransaction                                          => ist.dApp
        case other                                                                 => throw new IllegalArgumentException(s"Not implemented for $other")
      }
    }

    def unapply(tm: TransactionMeta): Option[(Height, Transaction, Boolean)] =
      Some((tm.height, tm.transaction, tm.succeeded))

    def create(height: Height, transaction: Transaction, succeeded: Boolean)(
        loadStateChanges: Transaction => Option[InvokeScriptResult]
    ): TransactionMeta =
      transaction match {
        case ist: InvokeScriptTransaction =>
          Invoke(height, ist, succeeded, loadStateChanges(ist))

        case ist: EthereumTransaction if ist.payload.isInstanceOf[EthereumTransaction.Invocation] =>
          Invoke(height, ist, succeeded, loadStateChanges(ist))

        case _ =>
          Default(height, transaction, succeeded)
      }
  }

  def apply(
      maybeDiff: => Option[(Height, Diff)],
      db: DB,
      blockchain: Blockchain,
      utx: UtxPool,
      wallet: Wallet,
      publishTransaction: Transaction => Future[TracedResult[ValidationError, Boolean]],
      blockAt: Int => Option[(BlockMeta, Seq[Transaction])]
  ): CommonTransactionsApi = new CommonTransactionsApi {
    override def aliasesOfAddress(address: Address): Observable[(Height, CreateAliasTransaction)] = common.aliasesOfAddress(db, maybeDiff, address)

    override def transactionsByAddress(
        subject: Address,
        sender: Option[Address],
        transactionTypes: Set[TransactionType],
        fromId: Option[ByteStr] = None
    ): Observable[TransactionMeta] =
      common.addressTransactions(db, maybeDiff, subject, sender, transactionTypes, fromId)

    override def transactionById(transactionId: ByteStr): Option[TransactionMeta] =
      blockchain.transactionInfo(transactionId).map {
        case (height, transaction, succeeded) =>
          TransactionMeta.create(Height(height), transaction, succeeded) { _ =>
            maybeDiff
              .flatMap { case (_, diff) => diff.scriptResults.get(transactionId) }
              .orElse(AddressTransactions.loadInvokeScriptResult(db, transactionId))
          }
      }

    override def unconfirmedTransactions: Seq[Transaction] = utx.all

    override def unconfirmedTransactionById(transactionId: ByteStr): Option[Transaction] =
      utx.transactionById(transactionId)

    override def calculateFee(tx: Transaction): Either[ValidationError, (Asset, Long, Long)] = {
      val defaultFee = FeeValidation.getMinFee(blockchain, tx)
      (tx match {
        case ist: InvokeScriptTransaction =>
          InvokeScriptTransactionDiff.calculateFee(blockchain, ist) match {
            case Some(wavesFee) => Right(FeeValidation.calculateAssetFee(blockchain, ist.feeAssetId, wavesFee))
            case None           => defaultFee
          }
        case _ => defaultFee
      }).map {
        case FeeDetails(asset, _, feeInAsset, feeInWaves) =>
          (asset, feeInAsset, feeInWaves)
      }
    }

    override def broadcastTransaction(tx: Transaction): Future[TracedResult[ValidationError, Boolean]] = publishTransaction(tx)

    override def transactionProofs(transactionIds: List[ByteStr]): List[TransactionProof] =
      for {
        transactionId            <- transactionIds
        (height, transaction, _) <- blockchain.transactionInfo(transactionId)
        (meta, allTransactions)  <- blockAt(height) if meta.header.version >= Block.ProtoBlockVersion
        transactionProof         <- block.transactionProof(transaction, allTransactions)
      } yield transactionProof
  }
}
