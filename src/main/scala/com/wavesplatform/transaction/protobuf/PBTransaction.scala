package com.wavesplatform.transaction.protobuf
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.serialization.protobuf.ProtobufTransactionParser
import com.wavesplatform.serialization.protobuf.utils.PBUtils
import com.wavesplatform.transaction.{AssetId, FastHashId, Proofs, SignedTransaction, TransactionParser, Transaction => VanillaTransaction}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scalapb.json4s.JsonFormat

trait PBTransaction extends VanillaTransaction with SignedTransaction with FastHashId { self: Transaction =>
  override def proofs: Proofs                    = new Proofs(self.getWrappedProofs.proofs.map(_.data).toList)
  override val signature: ByteStr                = self.getWrappedProofs.proofs.headOption.fold(ByteStr.empty)(_.data)
  override val sender: PublicKeyAccount          = self.getBody.sender
  override def builder: TransactionParser        = ProtobufTransactionParser
  override def assetFee: (Option[AssetId], Long) = (Some(self.getBody.feeAssetId).filterNot(_.isEmpty), self.getBody.fee)
  override def timestamp: Long                   = self.getBody.timestamp
  override val bodyBytes: Coeval[Array[Byte]]    = Coeval.evalOnce(self.body.fold(Array.emptyByteArray)(PBUtils.encodeDeterministic))
  override val bytes: Coeval[Array[Byte]]        = Coeval.evalOnce(self.toByteArray)
  override val json: Coeval[JsObject]            = Coeval.evalOnce(Json.parse(JsonFormat.toJsonString(self)).as[JsObject])
}
