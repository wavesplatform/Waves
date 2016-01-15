package scorex.consensus.qora

import com.google.common.primitives.{Bytes, Longs}
import play.api.libs.json.{JsObject, Json}
import scorex.block.BlockField
import scorex.crypto.encode.Base58


case class QoraConsensusBlockField(override val value: QoraLikeConsensusBlockData)
  extends BlockField[QoraLikeConsensusBlockData] {

  override val name: String = "qora-consensus"

  override def bytes: Array[Byte] =
    Bytes.ensureCapacity(Longs.toByteArray(value.generatingBalance), 8, 0) ++
      value.generatorSignature


  override def json: JsObject = Json.obj(name -> Json.obj(
    "base-target" -> value.generatingBalance,
    "generation-signature" -> Base58.encode(value.generatorSignature)
  ))
}
