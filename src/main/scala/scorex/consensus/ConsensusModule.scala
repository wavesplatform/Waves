package scorex.consensus

import scorex.block.{QoraGenesisBlock, NxtGenesisBlock, GenesisBlock}
import scorex.consensus.nxt.{NxtBlockGenerationDataParser, NxtBlockGenerationFunctions, NxtBlockGenerationData}
import scorex.consensus.qora.{QoraBlockGenerationData, QoraBlockGenerationDataParser, QoraBlockGenerationFunctions}

//consensus-related
//todo: stricter typing solution?
trait ConsensusModule {
  type kernelData <: BlockGenerationData

  val consensusFunctions: BlockGenerationFunctions
  val kernelDataParser: BlockGenerationDataParser[kernelData]

  val genesisBlock:GenesisBlock
}

object ConsensusModuleQora extends ConsensusModule {
  override type kernelData = QoraBlockGenerationData

  override val consensusFunctions = QoraBlockGenerationFunctions
  override val kernelDataParser = QoraBlockGenerationDataParser
  override val genesisBlock = QoraGenesisBlock

}

object ConsensusModuleNxt extends ConsensusModule {
  override type kernelData = NxtBlockGenerationData

  override val consensusFunctions = NxtBlockGenerationFunctions
  override val kernelDataParser = NxtBlockGenerationDataParser
  override val genesisBlock = NxtGenesisBlock
}
