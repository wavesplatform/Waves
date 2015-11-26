package scorex.perma.consensus

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Reads, Writes}

case class PermaLikeConsensusBlockData(difficulty: Long, puz: Array[Byte], ticket: Ticket)

object PermaLikeConsensusBlockData {
  implicit val writes: Writes[PermaLikeConsensusBlockData] = (
    (JsPath \ "difficulty").write[Long] and
      (JsPath \ "puz").write[Array[Byte]] and
      (JsPath \ "ticket").write[Ticket]
    ) (unlift(PermaLikeConsensusBlockData.unapply))

  implicit val reads: Reads[PermaLikeConsensusBlockData] = (
    (JsPath \ "difficulty").read[Long] and
      (JsPath \ "puz").read[Array[Byte]] and
      (JsPath \ "ticket").read[Ticket]
    ) (PermaLikeConsensusBlockData.apply _)

}