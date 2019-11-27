package com.wavesplatform.api.common

import com.wavesplatform.account.Address
import com.wavesplatform.api.http.ApiError.InvalidSignature
import com.wavesplatform.block.merkle.Merkle.TransactionProof
import com.wavesplatform.block.{Block, BlockMerkleOps}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base64
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.transaction.VanillaTransaction
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.state.diffs.FeeValidation.FeeDetails
import com.wavesplatform.state.{Blockchain, Height}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import monix.reactive.Observable
import play.api.libs.json._

private[api] class CommonTransactionsApi(
    blockchain: Blockchain,
    utx: UtxPool,
    wallet: Wallet,
    publishTransaction: VanillaTransaction => TracedResult[ValidationError, Boolean]
) {
  def transactionsByAddress(address: Address, fromId: Option[ByteStr] = None): Observable[(Height, VanillaTransaction)] =
    blockchain.addressTransactionsObservable(address, Set.empty, fromId)

  def transactionById(transactionId: ByteStr): Option[(Int, VanillaTransaction)] =
    blockchain.transactionInfo(transactionId)

  def unconfirmedTransactions(): Seq[VanillaTransaction] =
    utx.all

  def unconfirmedTransactionById(transactionId: ByteStr): Option[VanillaTransaction] =
    utx.transactionById(transactionId)

  def calculateFee(tx: VanillaTransaction): Either[ValidationError, (Asset, Long, Long)] =
    FeeValidation
      .getMinFee(blockchain, tx)
      .map {
        case FeeDetails(asset, _, feeInAsset, feeInWaves) =>
          (asset, feeInAsset, feeInWaves)
      }

  def transactionProofs(transactionIds: List[ByteStr]): List[TransactionProof] =
    for {
      transactionId         <- transactionIds
      (height, transaction) <- transactionById(transactionId)
      block                 <- blockchain.blockAt(height) if block.header.version >= Block.ProtoBlockVersion
      transactionProof      <- block.transactionProof(transaction)
    } yield transactionProof

  def broadcastTransaction(tx: VanillaTransaction): TracedResult[ValidationError, Boolean] = publishTransaction(tx)
}

private[api] object CommonTransactionsApi {

  def proofBytes(levels: Seq[Array[Byte]]): List[String] =
    (levels foldRight List.empty[String]) { case (d, acc) => s"${Base64.Prefix}${Base64.encode(d)}" :: acc }

  implicit val merkleInfoWrites: Writes[TransactionProof] = Writes { mi =>
    Json.obj(
      "id"               -> mi.id.toString,
      "transactionIndex" -> mi.transactionIndex,
      "merkleProof"      -> proofBytes(mi.digests)
    )
  }

  implicit val merkleInfoReads: Reads[TransactionProof] = Reads { jsv =>
    for {
      encoded          <- (jsv \ "id").validate[String]
      id               <- ByteStr.decodeBase58(encoded).fold(_ => JsError(InvalidSignature.message), JsSuccess(_))
      transactionIndex <- (jsv \ "transactionIndex").validate[Int]
      merkleProof      <- (jsv \ "merkleProof").validate[List[String]].map(_.map(Base64.decode))
    } yield TransactionProof(id, transactionIndex, merkleProof)
  }
}
