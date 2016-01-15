package scorex.perma.consensus

import play.api.libs.functional.syntax._
import play.api.libs.json._
import scorex.utils.JsonSerialization

case class PermaConsensusBlockData(target: BigInt, puz: Array[Byte], ticket: Ticket)

object PermaConsensusBlockData extends JsonSerialization {

  implicit val writes: Writes[PermaConsensusBlockData] = (
    (JsPath \ "difficulty").write[BigInt] and
      (JsPath \ "puz").write[Bytes] and
      (JsPath \ "ticket").write[Ticket]
    ) (unlift(PermaConsensusBlockData.unapply))

  implicit val reads: Reads[PermaConsensusBlockData] = (
    (JsPath \ "difficulty").read[BigInt] and
      (JsPath \ "puz").read[Bytes] and
      (JsPath \ "ticket").read[Ticket]
    ) (PermaConsensusBlockData.apply _)

}
