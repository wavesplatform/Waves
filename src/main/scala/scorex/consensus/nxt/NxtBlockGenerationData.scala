package scorex.consensus.nxt

import com.google.common.primitives.{Longs, Bytes}
import play.api.libs.json.{Json, JsObject}
import scorex.block.{NxtGenesisBlockGenerationData, Block}
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

  override def isGenesis: Boolean = baseTarget == NxtGenesisBlockGenerationData.InitialBaseTarget &&
    generatorSignature.sameElements(NxtGenesisBlockGenerationData.InitialGenerationSignature)

  override def signature(): Array[Byte] = generatorSignature.ensuring(_.length == NxtBlockGenerationDataParser.GENERATOR_SIGNATURE_LENGTH)

  //todo: implement sig checking!
  override def isSignatureValid(block: Block): Boolean = true
  //todo: implement valid checking!
  override def isValid(block: Block): Boolean = true

  override def blockScore() = BigInt("18446744073709551616") / baseTarget
}