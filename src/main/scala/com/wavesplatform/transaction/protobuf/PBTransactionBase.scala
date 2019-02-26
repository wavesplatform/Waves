package com.wavesplatform.transaction.protobuf
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

trait PBTransactionBase { tx: PBTransaction =>
  val protoBytes         = Coeval.evalOnce(PBTransactionSerialization.bytesWithTypePrefix(this))
  val protoUnsignedBytes = Coeval.evalOnce(PBTransactionSerialization.unsignedBytes(this))
  val protoJson          = Coeval.evalOnce(Json.toJson(this).as[JsObject])

  override def toString: String = Json.stringify(protoJson())
}
