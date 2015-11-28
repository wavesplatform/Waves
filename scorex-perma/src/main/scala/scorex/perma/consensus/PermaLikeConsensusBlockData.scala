package scorex.perma.consensus

import play.api.libs.functional.syntax._
import play.api.libs.json._
import scorex.utils.JsonSerialization

case class PermaLikeConsensusBlockData(target: BigInt, puz: Array[Byte], ticket: Ticket)

object PermaLikeConsensusBlockData extends JsonSerialization {

  implicit val writes: Writes[PermaLikeConsensusBlockData] = (
    (JsPath \ "difficulty").write[BigInt] and
      (JsPath \ "puz").write[Array[Byte]] and
      (JsPath \ "ticket").write[Ticket]
    ) (unlift(PermaLikeConsensusBlockData.unapply))

  implicit val reads: Reads[PermaLikeConsensusBlockData] = (
    (JsPath \ "difficulty").read[BigInt] and
      (JsPath \ "puz").read[Array[Byte]] and
      (JsPath \ "ticket").read[Ticket]
    ) (PermaLikeConsensusBlockData.apply _)

}