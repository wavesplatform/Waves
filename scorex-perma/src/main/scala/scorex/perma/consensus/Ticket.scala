package scorex.perma.consensus

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Reads, Writes}
import scorex.crypto.SigningFunctions._

case class Ticket(publicKey: PublicKey,
                  s: Array[Byte],
                  proofs: IndexedSeq[PartialProof])


object Ticket {
  implicit val writes: Writes[Ticket] = (
    (JsPath \ "publicKey").write[PublicKey] and
      (JsPath \ "s").write[Array[Byte]] and
      (JsPath \ "proofs").write[IndexedSeq[PartialProof]]
    ) (unlift(Ticket.unapply))

  implicit val reads: Reads[Ticket] = (
    (JsPath \ "publicKey").read[PublicKey] and
      (JsPath \ "s").read[Array[Byte]] and
      (JsPath \ "proofs").read[IndexedSeq[PartialProof]]
    ) (Ticket.apply _)

}
