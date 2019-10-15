package com.wavesplatform.transaction

import play.api.libs.json.{JsString, JsValue}

// todo: (NODE-1915) from SignedTransaction
trait SignatureField extends ProvenTransaction { self: VersionedTransaction =>
  override def proofField: Seq[(String, JsValue)] = {
    super.proofField ++ (if (self.version == 1.toByte) Seq("signature" -> JsString(this.proofs.toSignature.toString)) else Seq())
  }

  def isVersion1: Boolean = self.version == 1.toByte
}
