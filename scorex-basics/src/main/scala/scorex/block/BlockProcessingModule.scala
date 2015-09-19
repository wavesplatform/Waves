package scorex.block

/**
 * A generic interface having functions to convert data into a part of a block and vice versa.
 */

trait BlockProcessingModule[BlockPartDataType] {
  def parseBlockData(bytes: Array[Byte]): BlockField[BlockPartDataType]

  def parseBlockFields(blockFields: BlockField[BlockPartDataType]): BlockPartDataType = blockFields.value

  def genesisData: BlockField[BlockPartDataType]

  def formBlockData(data: BlockPartDataType): BlockField[BlockPartDataType]
}
