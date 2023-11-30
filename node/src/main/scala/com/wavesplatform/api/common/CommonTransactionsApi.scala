package com.wavesplatform.api.common

import com.wavesplatform.account.Address
import com.wavesplatform.api.{BlockMeta, common}
import com.wavesplatform.block
import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.TransactionProof
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.RDB
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.BlockChallenger
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.state.diffs.FeeValidation.FeeDetails
import com.wavesplatform.state.{Blockchain, Height, StateSnapshot, TxMeta}
import com.wavesplatform.transaction.TransactionType.TransactionType
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.{Asset, CreateAliasTransaction, Transaction}
import com.wavesplatform.utx.UtxPool
import monix.reactive.Observable

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
      maybeDiff: => Option[(Height, StateSnapshot)],
      rdb: RDB,
      blockchain: Blockchain,
      utx: UtxPool,
      blockChallenger: Option[BlockChallenger],
      publishTransaction: Transaction => Future[TracedResult[ValidationError, Boolean]],
      blockAt: Int => Option[(BlockMeta, Seq[(TxMeta, Transaction)])]
  ): CommonTransactionsApi = new CommonTransactionsApi {
    override def aliasesOfAddress(address: Address): Observable[(Height, CreateAliasTransaction)] =
      common.aliasesOfAddress(rdb, maybeDiff, address)

    override def transactionsByAddress(
        subject: Address,
        sender: Option[Address],
        transactionTypes: Set[TransactionType],
        fromId: Option[ByteStr] = None
    ): Observable[TransactionMeta] =
      common.addressTransactions(rdb, maybeDiff, subject, sender, transactionTypes, fromId)

    override def transactionById(transactionId: ByteStr): Option[TransactionMeta] =
      blockchain.transactionInfo(transactionId).map(common.loadTransactionMeta(rdb, maybeDiff))

    override def unconfirmedTransactions: Seq[Transaction] =
      utx.all ++ blockChallenger.fold(Seq.empty[Transaction])(_.allProcessingTxs)

    override def unconfirmedTransactionById(transactionId: ByteStr): Option[Transaction] =
      utx.transactionById(transactionId).orElse(blockChallenger.flatMap(_.getProcessingTx(transactionId)))

    override def calculateFee(tx: Transaction): Either[ValidationError, (Asset, Long, Long)] =
      FeeValidation
        .getMinFee(blockchain, tx)
        .map { case FeeDetails(asset, _, feeInAsset, feeInWaves) =>
          (asset, feeInAsset, feeInWaves)
        }

    override def broadcastTransaction(tx: Transaction): Future[TracedResult[ValidationError, Boolean]] = publishTransaction(tx)

    override def transactionProofs(transactionIds: List[ByteStr]): List[TransactionProof] =
      for {
        transactionId           <- transactionIds
        (txm, tx)               <- blockchain.transactionInfo(transactionId)
        (meta, allTransactions) <- blockAt(txm.height) if meta.header.version >= Block.ProtoBlockVersion
        transactionProof        <- block.transactionProof(tx, allTransactions.map(_._2))
      } yield transactionProof
  }
}
