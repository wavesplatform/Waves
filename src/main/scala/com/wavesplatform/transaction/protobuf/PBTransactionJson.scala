package com.wavesplatform.transaction.protobuf
import play.api.libs.json._
import scalapb.json4s.JsonFormat

//noinspection TypeAnnotation
trait PBTransactionJson {
  implicit val PBTransactionFormat = Format[Transaction] (
    Reads { js =>
      JsSuccess(JsonFormat.fromJsonString(Json.stringify(js)))
    },
    Writes { tx =>
      Json.parse(JsonFormat.toJsonString(tx))
    }
  )
}
