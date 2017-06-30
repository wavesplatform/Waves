package scorex.transaction

import com.wavesplatform.state2.ByteStr
import scorex.block.{Block, MicroBlock}
import scorex.utils.Synchronized

trait BlockchainUpdater extends Synchronized {
  def processBlock(block: Block): Either[ValidationError, Unit]

  def processMicroBlock(microBlock: MicroBlock): Either[ValidationError, Unit]

  def removeAfter(blockId: ByteStr): Either[ValidationError, BigInt]
}

