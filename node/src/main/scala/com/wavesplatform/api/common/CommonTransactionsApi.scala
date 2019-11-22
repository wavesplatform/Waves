package com.wavesplatform.api.common

import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.CommonTransactionsApi.MerkleInfo
import com.wavesplatform.block.{Block, BlockMerkleOps}
import com.wavesplatform.common.state.ByteStr
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
import play.api.libs.json.{Json, Writes}
import scorex.crypto.authds.merkle.MerkleProof
import scorex.crypto.hash.Digest32

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

  def transactionsMerkleInfo(transactionIds: List[ByteStr]): List[MerkleInfo] =
    for {
      transactionId         <- transactionIds
      (height, transaction) <- transactionById(transactionId)
      block                 <- blockchain.blockAt(height) if block.header.version >= Block.ProtoBlockVersion
      merkleProof      = block.transactionProof(transaction).get
      transactionHash  = merkleProof.leafData
      transactionsRoot = block.header.transactionsRoot
    } yield MerkleInfo(transactionId, ByteStr(transactionHash), transactionsRoot, merkleProof)

  def broadcastTransaction(tx: VanillaTransaction): TracedResult[ValidationError, Boolean] = publishTransaction(tx)
}

private[api] object CommonTransactionsApi {
  case class MerkleInfo(
      id: ByteStr,
      transactionHash: ByteStr,
      transactionsRoot: ByteStr,
      merkleProof: MerkleProof[Digest32]
  )

  object MerkleInfo {

    def proofBytes(proof: MerkleProof[Digest32]): Array[Byte] =
      (proof.levels foldLeft Array.emptyByteArray) {
        case (acc, (d, s)) =>
          Bytes.concat(acc, Array(s), Ints.toByteArray(d.size), d)
      }

    implicit val merkleInfoWrites: Writes[MerkleInfo] = Writes { mi =>
      Json.obj(
        "id"               -> mi.id.toString,
        "transactionHash"  -> mi.transactionHash.toString,
        "transactionsRoot" -> mi.transactionsRoot.toString,
        "merkleProof"      -> ByteStr(proofBytes(mi.merkleProof)).toString
      )
    }
  }
}
