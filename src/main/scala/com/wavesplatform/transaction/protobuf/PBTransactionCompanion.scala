package com.wavesplatform.transaction.protobuf
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.serialization.protobuf.ProtobufTransactionParser
import com.wavesplatform.serialization.protobuf.utils.PBUtils
import com.wavesplatform.transaction.{AssetId, FastHashId, SignedTransaction, TransactionParser}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scalapb.json4s.JsonFormat

trait PBTransactionCompanion {
  implicit class SignedPBTransaction(tx: Transaction) extends SignedTransaction with FastHashId {
    override lazy val signature: ByteStr = tx.getProofs.proofs.headOption.fold(ByteStr.empty)(_.data)
    override val sender: PublicKeyAccount          = tx.getBody.sender
    override def builder: TransactionParser        = ProtobufTransactionParser
    override def assetFee: (Option[AssetId], Long) = (Some(tx.getBody.feeAssetId).filterNot(_.isEmpty), tx.getBody.fee)
    override def timestamp: Long                   = tx.getBody.timestamp
    override val bodyBytes: Coeval[Array[Byte]]    = Coeval.evalOnce(tx.body.fold(Array.emptyByteArray)(PBUtils.encodeDeterministic))
    override val bytes: Coeval[Array[Byte]]        = Coeval.evalOnce(tx.toByteArray)
    override val json: Coeval[JsObject]            = Coeval.evalOnce(Json.parse(JsonFormat.toJsonString(tx)).as[JsObject])

    def legacy: SignedPBTransaction = this
  }
}
