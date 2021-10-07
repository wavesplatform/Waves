package com.wavesplatform.api.common

import com.wavesplatform.account.Address
import com.wavesplatform.api.{BlockMeta, common}
import com.wavesplatform.block
import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.TransactionProof
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.state.diffs.FeeValidation.FeeDetails
import com.wavesplatform.state.{Blockchain, Diff, Height}
import com.wavesplatform.transaction.TransactionType.TransactionType
import com.wavesplatform.transaction.smart.InvokeTransaction
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.{Asset, CreateAliasTransaction, Transaction}
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import monix.reactive.Observable
import org.iq80.leveldb.DB

import scala.concurrent.Future

trait CommonTransactionsApi {

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
      blockchain.transactionInfo(transactionId).map(common.loadTransactionMeta(db, maybeDiff))

    override def unconfirmedTransactions: Seq[Transaction] = utx.all

    override def unconfirmedTransactionById(transactionId: ByteStr): Option[Transaction] =
      utx.transactionById(transactionId)

    override def calculateFee(tx: Transaction): Either[ValidationError, (Asset, Long, Long)] = {
      val defaultFee = FeeValidation.getMinFee(blockchain, tx)
      (tx match {
        case ist: InvokeTransaction => FeeValidation.calculateInvokeFee(blockchain, ist).fold(defaultFee)(Right(_))
        case _                      => defaultFee
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
