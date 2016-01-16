package scorex.block

import scala.util.Try

/**
  * A generic interface with functionality to convert data into a part of a block and vice versa
  */

trait BlockProcessingModule[BlockPartDataType] {
  def parseBlockData(bytes: Array[Byte]): Try[BlockField[BlockPartDataType]]

  def parseBlockFields(blockFields: BlockField[BlockPartDataType]): BlockPartDataType = blockFields.value

  def genesisData: BlockField[BlockPartDataType]

  def formBlockData(data: BlockPartDataType): BlockField[BlockPartDataType]
}
