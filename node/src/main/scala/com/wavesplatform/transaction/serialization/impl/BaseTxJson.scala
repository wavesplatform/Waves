package com.wavesplatform.transaction.serialization.impl

import com.wavesplatform.transaction.{PBSince, ProvenTransaction, SigProofsSwitch, Transaction, VersionedTransaction}
import play.api.libs.json.{JsArray, JsObject, JsString, Json}

object BaseTxJson {
  def toJson(tx: Transaction with ProvenTransaction): JsObject = {
    import tx._
    Json.obj(
      "type"            -> tpe.id,
      "id"              -> id().toString,
      "sender"          -> sender.toAddress.toString,
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
      case pbs: PBSince if pbs.isProtobufVersion => Json.obj("chainId" -> tx.chainId)
      case _                                            => Json.obj()
    })
  }
}
