package scorex.block

/**
  * A generic interface having functions to convert data into a part of a block and vice versa.
  */

trait BlockProcessingModule[MT] {
  def parseBlockData(bytes: Array[Byte]): BlockField[MT]

  def parseBlockFields(blockFields: BlockField[MT]): MT =
    blockFields.value

  def genesisData: BlockField[MT]

  def formBlockData(data: MT): BlockField[MT]
}
