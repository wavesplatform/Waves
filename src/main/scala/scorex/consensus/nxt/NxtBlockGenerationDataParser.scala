package scorex.consensus.nxt

import com.google.common.primitives.Longs
import scorex.consensus.BlockGenerationDataParser

object NxtBlockGenerationDataParser extends BlockGenerationDataParser[NxtBlockGenerationData] {
  val BaseTargetLength = 8

  override val GeneratorSignatureLength = 32

  override val GenerationDataLength: Int = GeneratorSignatureLength + BaseTargetLength

  override def parse(bytes: Array[Byte]): NxtBlockGenerationData = {
    require(bytes.length == GenerationDataLength)

    val baseTarget = Longs.fromByteArray(bytes.take(BaseTargetLength))
    val generatorSignature = bytes.drop(BaseTargetLength)
    new NxtBlockGenerationData(baseTarget, generatorSignature)
  }
}