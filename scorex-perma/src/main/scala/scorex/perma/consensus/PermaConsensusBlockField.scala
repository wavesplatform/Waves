package scorex.perma.consensus

import play.api.libs.json._
import scorex.block.BlockField


case class PermaConsensusBlockField(override val value: PermaLikeConsensusBlockData)
  extends BlockField[PermaLikeConsensusBlockData] {


  override val name: String = PermaConsensusBlockField.fieldName

  override def bytes: Array[Byte] = json.toString().getBytes

  override def json: JsObject = Json.obj(name -> Json.toJson(value))
}

object PermaConsensusBlockField {

  val fieldName: String = "perma-consensus"

}
