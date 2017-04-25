package scorex.transaction

import com.wavesplatform.state2.reader.StateReader
import scorex.block.Block.BlockId
import scorex.crypto.encode.Base58
import scorex.utils.ScorexLogging

trait BlockStorage extends ScorexLogging {

  def history: History

  def stateReader: StateReader

  def checkpoints : CheckpointService

  def blockchainUpdater: BlockchainUpdater

}
