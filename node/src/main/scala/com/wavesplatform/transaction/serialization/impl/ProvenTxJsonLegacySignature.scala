package com.wavesplatform.transaction.serialization.impl

import com.wavesplatform.transaction.{ProvenTransaction, SignedTransaction, VersionedTransaction}
import com.wavesplatform.transaction.serialization.TxJson
import play.api.libs.json.{JsObject, Json}

object ProvenTxJsonLegacySignature extends TxJson[ProvenTransaction] {
  override def toJson(tx: ProvenTransaction): JsObject = {
    Json.obj("signature" -> tx.signature.toString)
  }

  def onlyV1(tx: VersionedTransaction with ProvenTransaction): JsObject =
    if (tx.version == 1) toJson(tx) else Json.obj()
}
