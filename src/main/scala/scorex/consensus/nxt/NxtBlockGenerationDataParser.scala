package scorex.consensus.nxt

import scorex.consensus.BlockGenerationDataParser


object NxtBlockGenerationDataParser extends BlockGenerationDataParser[NxtBlockGenerationData]{
  val BASE_TARGET_LENGTH = 8
  val GENERATOR_SIGNATURE_LENGTH = 32

  val GENERATION_DATA_LENGTH:Int = GENERATOR_SIGNATURE_LENGTH + BASE_TARGET_LENGTH

  override def parse(bytes: Array[Byte]): NxtBlockGenerationData = ???
}
