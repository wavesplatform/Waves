package scorex.api.http

import play.api.libs.json.Reads
import scorex.api.http.assets.TransferRequest.transferFormat
import scorex.api.http.assets.VersionedTransferRequest.versionedTransferFormat
import shapeless.{:+:, CNil, Coproduct}

package object assets {

  type TransferRequests = TransferRequest :+: VersionedTransferRequest :+: CNil
  implicit val autoVersionTransferRequestsReads: Reads[TransferRequests] = Reads { json =>
    (json \ "version").asOpt[Byte] match {
      case None => transferFormat.reads(json).map(Coproduct[TransferRequests](_))
      case _    => versionedTransferFormat.reads(json).map(Coproduct[TransferRequests](_))
    }
  }

}
