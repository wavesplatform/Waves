package scorex.perma.consensus

import play.api.libs.json.{JsObject, Json}
import scorex.block.BlockField
import scorex.crypto.Base58
import scorex.perma.actors.Ticket


case class PermaConsensusBlockField(override val value: PermaLikeConsensusBlockData)
  extends BlockField[PermaLikeConsensusBlockData] {

  override val name: String = "perma-consensus"

  override def bytes: Array[Byte] = {
    //todo: implement
    ???
  }

  //todo: implement
  override def json: JsObject = Json.obj(name -> Json.obj(
    "difficulty" -> value.difficulty.toString()
  ))
}
