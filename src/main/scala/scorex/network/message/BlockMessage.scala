package scorex.network.message

import com.google.common.primitives.{Bytes, Ints}
import scorex.block.Block


case class BlockMessage(height: Int, block: Block) extends Message {
  override val messageType = Message.BlockType

  override lazy val dataBytes = {
    val heightBytes = Ints.toByteArray(block.height().get)
    val blockBytes = block.toBytes
    Bytes.concat(heightBytes, blockBytes)
  }
}


object BlockMessage {

  private val HEIGHT_LENGTH = 4

  def apply(data: Array[Byte]): BlockMessage = {
    val heightBytes = data.take(HEIGHT_LENGTH)
    val height = Ints.fromByteArray(heightBytes)

    val block = Block.parse(data.drop(HEIGHT_LENGTH)).get

    new BlockMessage(height, block)
  }
}