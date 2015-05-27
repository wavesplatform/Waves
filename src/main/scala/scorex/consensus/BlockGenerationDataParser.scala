package scorex.consensus


trait BlockGenerationDataParser[KDT <: BlockGenerationData] {
  val GENERATOR_SIGNATURE_LENGTH: Int
  val GENERATION_DATA_LENGTH: Int

  def parse(bytes: Array[Byte]): KDT
}
