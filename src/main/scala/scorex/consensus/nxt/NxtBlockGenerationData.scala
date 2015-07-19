package scorex.consensus.nxt

import com.google.common.primitives.{Bytes, Longs}
import play.api.libs.json.{JsObject, Json}
import scorex.block.{Block, NxtGenesisBlockGenerationData}
import scorex.consensus.BlockGenerationData
import scorex.crypto.Base58

import scala.util.Try

case class NxtBlockGenerationData(baseTarget: Long, generatorSignature: Array[Byte])
  extends BlockGenerationData {

  override def bytes: Array[Byte] = Bytes.concat(
    Longs.toByteArray(baseTarget),
    generatorSignature
  ).ensuring(_.length == NxtBlockGenerationDataParser.GenerationDataLength)

  override def json: JsObject = Json.obj(
    "baseTarget" -> baseTarget,
    "generatorSignature" -> Base58.encode(generatorSignature)
  )

  override def isGenesis: Boolean = baseTarget == NxtGenesisBlockGenerationData.InitialBaseTarget &&
    generatorSignature.sameElements(NxtGenesisBlockGenerationData.InitialGenerationSignature)

  override def signature(): Array[Byte] = generatorSignature.ensuring(_.length == NxtBlockGenerationDataParser.GeneratorSignatureLength)

  override def isValid(block: Block): Boolean = Try {
    import NxtBlockGenerationFunctions._

    val gd = block.generationData.asInstanceOf[NxtBlockGenerationData]
    val blockTime = block.timestamp

    val prev = block.parent().get
    val prevTime = prev.timestamp

    val prevGd = prev.generationData.asInstanceOf[NxtBlockGenerationData]
    val generator = block.generator

    //check baseTarget
    val cbt = calcBaseTarget(prevGd, prevTime, blockTime)
    require(cbt == baseTarget, "Block's basetarget is wrong")

    //check generation signature
    val calcGs = calcGeneratorSignature(prevGd, generator)
    require(calcGs.sameElements(generatorSignature), "Block's generation signature is wrong")

    //check hit < target
    calcHit(prevGd, generator) < calcTarget(prevGd, prevTime, generator)
  }.getOrElse(false)

  override def blockScore() = BigInt("18446744073709551616") / baseTarget
}