package com.wavesplatform.api.http

import play.api.libs.json._
import shapeless.{:+:, CNil, Coproduct}
import supertagged.TaggedType

package object assets {

  type TransferRequests = TransferV1Request :+: TransferV2Request :+: CNil
  implicit val autoTransferRequestsReads: Reads[TransferRequests] = Reads { json =>
    (json \ "version").asOpt[Byte] match {
      case None => TransferV1Request.format.reads(json).map(Coproduct[TransferRequests](_))
      case _    => TransferV2Request.format.reads(json).map(Coproduct[TransferRequests](_))
    }
  }
  implicit val autoTransferRequestsWrites: Writes[TransferRequests] = Writes {
    _.eliminate(
      TransferV1Request.format.writes,
      _.eliminate(
        TransferV2Request.format.writes,
        _ => JsNull
      )
    )
  }

  type SignedTransferRequests = SignedTransferV1Request :+: SignedTransferV2Request :+: CNil
  implicit val autoSignedTransferRequestsReads: Reads[SignedTransferRequests] = Reads { json =>
    (json \ "version").asOpt[Int] match {
      case None | Some(1) => SignedTransferV1Request.reads.reads(json).map(Coproduct[SignedTransferRequests](_))
      case _              => SignedTransferV2Request.reads.reads(json).map(Coproduct[SignedTransferRequests](_))
    }
  }
  implicit val autoSignedTransferRequestsWrites: Writes[SignedTransferRequests] = Writes {
    _.eliminate(
      SignedTransferV1Request.writes.writes,
      _.eliminate(
        SignedTransferV2Request.writes.writes,
        _ => JsNull
      )
    )
  }

  object ProofStr extends TaggedType[String]
  type ProofStr = ProofStr.Type

  implicit object MaybeStringReads extends Reads[ProofStr] {
    override def reads(json: JsValue): JsResult[ProofStr] = {
      json match {
        case JsNull      => JsSuccess(ProofStr(""))
        case JsString(s) => JsSuccess(ProofStr(s))
        case _           => JsError(Seq(JsPath -> Seq(JsonValidationError("error.expected.jsstring"))))
      }
    }
  }
}
