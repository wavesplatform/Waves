package scorex.consensus.qora

import com.google.common.primitives.Longs
import scorex.consensus.BlockGenerationDataParser


object QoraBlockGenerationDataParser extends BlockGenerationDataParser[QoraBlockGenerationData] {
  val GENERATING_BALANCE_LENGTH = 8

  override val GENERATOR_SIGNATURE_LENGTH = 64

  override val GENERATION_DATA_LENGTH = GENERATING_BALANCE_LENGTH + GENERATOR_SIGNATURE_LENGTH

  def parse(bytes: Array[Byte]): QoraBlockGenerationData = {
    require(bytes.length == GENERATION_DATA_LENGTH)

    val generatingBalance = Longs.fromByteArray(bytes.take(GENERATING_BALANCE_LENGTH))
    val generatorSignature = bytes.drop(GENERATING_BALANCE_LENGTH)
    new QoraBlockGenerationData(generatingBalance, generatorSignature)
  }
}