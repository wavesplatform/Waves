package scorex.lagonaki.network.message

import com.google.common.primitives.{Bytes, Ints}
import scorex.block.Block
import scorex.consensus.ConsensusModule
import scorex.transaction.TransactionModule


case class BlockMessage(height: Int, block: Block) extends Message {
  import BlockMessage.HEIGHT_LENGTH

  override val messageType = Message.BlockType

  override lazy val dataBytes = {
    val heightBytes = Ints.toByteArray(height).ensuring(_.size == HEIGHT_LENGTH)
    val blockBytes = block.bytes
    Bytes.concat(heightBytes, blockBytes)
  }
}


object BlockMessage {

  private val HEIGHT_LENGTH = 4

  def apply(data: Array[Byte])
           (implicit consensusModule: ConsensusModule[_],
            transactionModule: TransactionModule[_]): BlockMessage = {
    val heightBytes = data.take(HEIGHT_LENGTH)
    val height = Ints.fromByteArray(heightBytes)

    val block = Block.parse(data.drop(HEIGHT_LENGTH)).get

    new BlockMessage(height, block)
  }
}