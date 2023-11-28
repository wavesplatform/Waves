package com.wavesplatform.transaction.serialization.impl

import com.wavesplatform.transaction.{EthereumTransaction, PBSince, ProvenTransaction, SigProofsSwitch, Transaction, Versioned}
import play.api.libs.json.{JsArray, JsObject, JsString, Json}

object BaseTxJson {
  def toJson(tx: Transaction): JsObject = {
    Json.obj(
      "type"       -> tx.tpe.id,
      "id"         -> tx.id().toString,
      "fee"        -> tx.assetFee._2,
      "feeAssetId" -> tx.assetFee._1.maybeBase58Repr,
      "timestamp"  -> tx.timestamp
    ) ++ (tx match {
      case v: Versioned           => Json.obj("version" -> v.version)
      case _: EthereumTransaction => Json.obj("version" -> 1)
      case _                      => Json.obj()
    }) ++ (tx match {
      case pbs: PBSince with Versioned if PBSince.affects(pbs) => Json.obj("chainId" -> tx.chainId)
      case e: EthereumTransaction                              => Json.obj("chainId" -> e.chainId)
      case _                                                   => Json.obj()
    }) ++ (tx match {
      case p: ProvenTransaction =>
        Json.obj(
          "sender"          -> p.sender.toAddress(p.chainId),
          "senderPublicKey" -> p.sender,
          "proofs"          -> JsArray(p.proofs.proofs.map(p => JsString(p.toString)))
        ) ++ (tx match {
          // Compatibility
          case s: SigProofsSwitch if s.usesLegacySignature => Json.obj("signature" -> s.signature.toString)
          case _                                           => Json.obj()
        })
      case _ => JsObject.empty
    })
  }
}
