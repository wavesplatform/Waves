package scorex.transaction

import com.wavesplatform.state2.ByteStr
import scorex.block.Block
import scorex.utils.Synchronized

trait BlockchainUpdater extends Synchronized {
  def processBlock(block: Block): Either[ValidationError, Unit]

  def removeAfter(blockId: ByteStr): Boolean
}

