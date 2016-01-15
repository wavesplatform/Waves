package scorex.perma.consensus

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Reads, Writes}
import scorex.crypto.singing.SigningFunctions.PublicKey
import scorex.utils.JsonSerialization

case class Ticket(publicKey: PublicKey,
                  s: Array[Byte],
                  proofs: IndexedSeq[PartialProof])


object Ticket extends JsonSerialization {
  implicit val writes: Writes[Ticket] = (
    (JsPath \ "publicKey").write[PublicKey] and
      (JsPath \ "s").write[Bytes] and
      (JsPath \ "proofs").write[IndexedSeq[PartialProof]]
    ) (unlift(Ticket.unapply))

  implicit val reads: Reads[Ticket] = (
    (JsPath \ "publicKey").read[PublicKey] and
      (JsPath \ "s").read[Bytes] and
      (JsPath \ "proofs").read[IndexedSeq[PartialProof]]
    ) (Ticket.apply _)

}
