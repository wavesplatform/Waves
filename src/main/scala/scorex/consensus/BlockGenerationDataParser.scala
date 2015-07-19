package scorex.consensus


trait BlockGenerationDataParser[KDT <: BlockGenerationData] {
  val GeneratorSignatureLength: Int
  val GenerationDataLength: Int

  def parse(bytes: Array[Byte]): KDT
}
