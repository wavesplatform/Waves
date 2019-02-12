package com.wavesplatform.transaction.protobuf

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.serialization.protobuf.utils.PBUtils
import com.wavesplatform.transaction.{AssetId, FastHashId, Proofs, SignedTransaction, TransactionParser, Transaction => VanillaTransaction}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

trait PBTransaction extends VanillaTransaction with SignedTransaction with FastHashId { self: PBTransaction.TX with PBTransactionImplicits =>
  override val proofs                            = Proofs(self.proofsArray)
  override val signature: ByteStr                = self.proofs.proofs.headOption.getOrElse(ByteStr.empty)
  override def builder: TransactionParser        = self.companion
  override def assetFee: (Option[AssetId], Long) = (Some(self.feeAssetId).filterNot(_.isEmpty), self.fee)
  override val bodyBytes: Coeval[Array[Byte]]    = Coeval.evalOnce(self.version match {
    case 1 | 2 => PBTransactionImplicits.PBTransactionImplicitConversionOps(self).toVanilla.bodyBytes()
    case _ => PBUtils.encodeDeterministic(this.copy(proofsArray = Nil))
  })
  override val bytes: Coeval[Array[Byte]]        = Coeval.evalOnce(PBUtils.encodeDeterministic(this))
  override val json: Coeval[JsObject]            = Coeval.evalOnce(Json.toJson(self).as[JsObject])
}

object PBTransaction {
  final type TX = com.wavesplatform.transaction.protobuf.Transaction
  val TX = com.wavesplatform.transaction.protobuf.Transaction
}
