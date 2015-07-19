package scorex.consensus.qora

import com.google.common.primitives.Longs
import scorex.consensus.BlockGenerationDataParser


object QoraBlockGenerationDataParser extends BlockGenerationDataParser[QoraBlockGenerationData] {
  val GENERATING_BALANCE_LENGTH = 8

  override val GeneratorSignatureLength = 64

  override val GenerationDataLength = GENERATING_BALANCE_LENGTH + GeneratorSignatureLength

  def parse(bytes: Array[Byte]): QoraBlockGenerationData = {
    require(bytes.length == GenerationDataLength)

    val generatingBalance = Longs.fromByteArray(bytes.take(GENERATING_BALANCE_LENGTH))
    val generatorSignature = bytes.drop(GENERATING_BALANCE_LENGTH)
    new QoraBlockGenerationData(generatingBalance, generatorSignature)
  }
}