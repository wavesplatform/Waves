package scorex.perma.consensus

import play.api.libs.json.{JsObject, Json}
import scorex.block.BlockField
import scorex.crypto.Base58


case class PermaConsensusBlockField(override val value: PermaLikeConsensusBlockData)
  extends BlockField[PermaLikeConsensusBlockData] {


  override val name: String = "perma-consensus"

  override def bytes: Array[Byte] = {
    //todo: implement
    ???
  }

  override def json: JsObject = Json.obj(name -> Json.obj(
    "difficulty" -> value.difficulty.toString(),
    "puz" -> value.puz,
    "s" -> value.ticket.s,
    "segments" -> value.ticket.proofs.map { proof =>
      Json.obj(
        "data" -> proof.segment.data,
        "path" -> Json.arr(proof.segment.merklePath.map(Base58.encode))
      )
    }
  ))
}
