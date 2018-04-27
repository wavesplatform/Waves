package scorex.api.http

import play.api.libs.json._
import shapeless.{:+:, CNil, Coproduct}

package object assets {

  type TransferRequests = TransferRequest :+: VersionedTransferRequest :+: CNil
  implicit val autoTransferRequestsReads: Reads[TransferRequests] = Reads { json =>
    (json \ "version").asOpt[Byte] match {
      case None => TransferRequest.format.reads(json).map(Coproduct[TransferRequests](_))
      case _    => VersionedTransferRequest.format.reads(json).map(Coproduct[TransferRequests](_))
    }
  }
  implicit val autoTransferRequestsWrites: Writes[TransferRequests] = Writes {
    _.eliminate(
      TransferRequest.format.writes,
      _.eliminate(
        VersionedTransferRequest.format.writes,
        _ => JsNull
      )
    )
  }

  type SignedTransferRequests = SignedTransferRequest :+: SignedVersionedTransferRequest :+: CNil
  implicit val autoSignedTransferRequestsReads: Reads[SignedTransferRequests] = Reads { json =>
    (json \ "version").asOpt[Int] match {
      case None | Some(1) => SignedTransferRequest.reads.reads(json).map(Coproduct[SignedTransferRequests](_))
      case _              => SignedVersionedTransferRequest.format.reads(json).map(Coproduct[SignedTransferRequests](_))
    }
  }
  implicit val autoSignedTransferRequestsWrites: Writes[SignedTransferRequests] = Writes {
    _.eliminate(
      SignedTransferRequest.writes.writes,
      _.eliminate(
        SignedVersionedTransferRequest.format.writes,
        _ => JsNull
      )
    )
  }

}
