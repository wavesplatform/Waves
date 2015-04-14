package scorex.consensus

import scorex.block.Block


trait BlockGenerationFunctions {
  def generateBlock(): Option[Block]
}
