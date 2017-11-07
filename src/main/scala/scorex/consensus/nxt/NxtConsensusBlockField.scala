package scorex.consensus.nxt

import com.google.common.primitives.{Bytes, Longs}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.block.BlockField

case class NxtConsensusBlockField(override val value: NxtLikeConsensusBlockData)
  extends BlockField[NxtLikeConsensusBlockData] {

  override val name: String = "nxt-consensus"

  override val bytes = Coeval.evalOnce(
    Bytes.ensureCapacity(Longs.toByteArray(value.baseTarget), 8, 0) ++
      value.generationSignature.arr)


  override def json: JsObject = Json.obj(name -> Json.obj(
    "base-target" -> value.baseTarget,
    "generation-signature" -> value.generationSignature.base58
  ))
}
