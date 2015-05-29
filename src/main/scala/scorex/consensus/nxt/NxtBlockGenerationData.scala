package scorex.consensus.nxt

import com.google.common.primitives.{Bytes, Longs}
import play.api.libs.json.{JsObject, Json}
import scorex.block.{Block, NxtGenesisBlockGenerationData}
import scorex.consensus.BlockGenerationData
import scorex.crypto.Base58

case class NxtBlockGenerationData(baseTarget: Long, generatorSignature: Array[Byte])
  extends BlockGenerationData {

  override def toBytes: Array[Byte] = Bytes.concat(
    Longs.toByteArray(baseTarget),
    generatorSignature
  ).ensuring(_.length == NxtBlockGenerationDataParser.GENERATION_DATA_LENGTH)

  override def toJson: JsObject = Json.obj(
    "baseTarget" -> baseTarget,
    "generatorSignature" -> Base58.encode(generatorSignature)
  )

  override def isGenesis: Boolean = baseTarget == NxtGenesisBlockGenerationData.InitialBaseTarget &&
    generatorSignature.sameElements(NxtGenesisBlockGenerationData.InitialGenerationSignature)

  override def signature(): Array[Byte] = generatorSignature.ensuring(_.length == NxtBlockGenerationDataParser.GENERATOR_SIGNATURE_LENGTH)

  //todo: implement signature check!
  override def isSignatureValid(block: Block): Boolean = true

  //todo: implement validity check!
  override def isValid(block: Block): Boolean = true

  override def blockScore() = BigInt("18446744073709551616") / baseTarget
}