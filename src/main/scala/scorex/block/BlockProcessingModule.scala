package scorex.block

import scorex.serialization.Deser

import scala.util.Try

/**
  * A generic interface with functionality to convert data into a part of a block and vice versa
  */

trait BlockProcessingModule[BlockPartDataType] extends Deser[BlockField[BlockPartDataType]] {

}
