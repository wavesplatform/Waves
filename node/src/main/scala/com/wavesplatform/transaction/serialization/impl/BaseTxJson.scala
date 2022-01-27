package com.wavesplatform.transaction.serialization.impl

import com.wavesplatform.transaction.{LegacyPBSwitch, ProvenTransaction, SigProofsSwitch, VersionedTransaction}
import play.api.libs.json.{JsArray, JsObject, JsString, Json}

object BaseTxJson {
  def toJson(tx: ProvenTransaction): JsObject = {
    import tx._
    Json.obj(
      "type"            -> typeId,
      "id"              -> id().toString,
      "sender"          -> sender.toAddress(tx.chainId),
      "senderPublicKey" -> sender,
      "fee"             -> assetFee._2,
      "feeAssetId"      -> assetFee._1.maybeBase58Repr,
      "timestamp"       -> timestamp,
      "proofs"          -> JsArray(proofs.proofs.map(p => JsString(p.toString)))
    ) ++ (tx match {
      // Compatibility
      case s: SigProofsSwitch if s.usesLegacySignature => Json.obj("signature" -> tx.signature.toString)
      case _                                           => Json.obj()
    }) ++ (tx match {
      case v: VersionedTransaction => Json.obj("version" -> v.version)
      case _                       => Json.obj()
    }) ++ (tx match {
      case pbs: LegacyPBSwitch if pbs.isProtobufVersion => Json.obj("chainId" -> tx.chainId)
      case _                                            => Json.obj()
    })
  }
}
