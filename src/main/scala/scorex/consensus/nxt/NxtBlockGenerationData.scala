package scorex.consensus.nxt

import com.google.common.primitives.{Longs, Bytes}
import play.api.libs.json.{Json, JsObject}
import scorex.block.Block
import scorex.consensus.BlockGenerationData
import scorex.crypto.Base58

class NxtBlockGenerationData(val baseTarget: Long, val generatorSignature: Array[Byte])
  extends BlockGenerationData {

  override def toBytes: Array[Byte] = Bytes.concat(
    Longs.toByteArray(baseTarget),
    generatorSignature
  )

  override def toJson: JsObject = Json.obj(
    "baseTarget" -> baseTarget,
    "generatorSignature" -> Base58.encode(generatorSignature)
  )

  override def isGenesis: Boolean = ???

  override def signature: Array[Byte] = ???

  override def isSignatureValid(block: Block): Boolean = ???

  override def isValid(block: Block): Boolean = ???

  override def blockScore() = BigInt("18446744073709551616") / baseTarget
}