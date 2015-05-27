package scorex.consensus.nxt

import com.google.common.primitives.Longs
import scorex.consensus.BlockGenerationDataParser

object NxtBlockGenerationDataParser extends BlockGenerationDataParser[NxtBlockGenerationData] {
  val BASE_TARGET_LENGTH = 8

  override val GENERATOR_SIGNATURE_LENGTH = 32

  override val GENERATION_DATA_LENGTH: Int = GENERATOR_SIGNATURE_LENGTH + BASE_TARGET_LENGTH

  override def parse(bytes: Array[Byte]): NxtBlockGenerationData = {
    require(bytes.length == GENERATION_DATA_LENGTH)

    val baseTarget = Longs.fromByteArray(bytes.take(BASE_TARGET_LENGTH))
    val generatorSignature = bytes.drop(BASE_TARGET_LENGTH)
    new NxtBlockGenerationData(baseTarget, generatorSignature)
  }
}