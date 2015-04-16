package scorex.consensus


trait BlockGenerationDataParser[KDT <: BlockGenerationData] {
  val GENERATION_DATA_LENGTH:Int

  def parse(bytes:Array[Byte]):KDT
}
