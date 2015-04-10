package scorex.network.message

import java.util.Arrays

import com.google.common.primitives.{Bytes, Ints}
import scorex.block.Block


case class BlockMessage(height: Int, block: Block) extends Message {

  import scorex.network.message.BlockMessage._

  override val messageType = Message.BLOCK_TYPE

  override def toBytes() = {
    val heightBytes = Ints.toByteArray(block.height().get)
    val blockBytes = block.toBytes
    val data = Bytes.concat(heightBytes, blockBytes)
    Bytes.concat(super.toBytes(), this.generateChecksum(data), data)
  }

  override protected def getDataLength() = HEIGHT_LENGTH + block.dataLength()
}


object BlockMessage {

  private val HEIGHT_LENGTH = 4

  def apply(data: Array[Byte]): BlockMessage = {
    val heightBytes = Arrays.copyOfRange(data, 0, HEIGHT_LENGTH)
    val height = Ints.fromByteArray(heightBytes)

    val block = Block.parse(Arrays.copyOfRange(data, HEIGHT_LENGTH, data.length + 1)).get //todo: exceptions?

    new BlockMessage(height, block)
  }
}