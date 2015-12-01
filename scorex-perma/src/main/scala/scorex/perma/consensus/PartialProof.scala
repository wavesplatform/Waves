package scorex.perma.consensus

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Reads, Writes}
import scorex.crypto.SigningFunctions._
import scorex.crypto.ads.merkle.AuthDataBlock
import scorex.perma.settings.Constants._
import scorex.utils.JsonSerialization

case class PartialProof(signature: Signature, segmentIndex: Long, segment: AuthDataBlock[DataSegment])

object PartialProof extends JsonSerialization {
  implicit val writes: Writes[PartialProof] = (
    (JsPath \ "signature").write[Bytes] and
      (JsPath \ "segmentIndex").write[Long] and
      (JsPath \ "segment").write[AuthDataBlock[DataSegment]]
    ) (unlift(PartialProof.unapply))

  implicit val reads: Reads[PartialProof] = (
    (JsPath \ "signature").read[Bytes] and
      (JsPath \ "segmentIndex").read[Long] and
      (JsPath \ "segment").read[AuthDataBlock[DataSegment]]
    ) (PartialProof.apply _)
}
