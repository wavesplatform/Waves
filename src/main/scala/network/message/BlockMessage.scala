package network.message

import java.util.Arrays
import network.ConnectedPeer
import scorex.block.Block
import com.google.common.primitives.Bytes
import com.google.common.primitives.Ints

case class BlockMessage(height: Int, block: Block, mbSender: Option[ConnectedPeer] = None, mbId: Option[Int] = None) extends Message {

  import BlockMessage._


  override val messageType = Message.BLOCK_TYPE


  override def toBytes() = {
    val heightBytes = Ints.toByteArray(block.getHeight())
    val blockBytes = block.toBytes
    val data = Bytes.concat(heightBytes, blockBytes)
    Bytes.concat(super.toBytes(), this.generateChecksum(data), data)
  }

  override protected def getDataLength() = HEIGHT_LENGTH + block.getDataLength()
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