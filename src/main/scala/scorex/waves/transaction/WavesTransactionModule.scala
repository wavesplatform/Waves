package scorex.waves.transaction

import scorex.app.RunnableApplication
import scorex.block.BlockField
import scorex.settings.{ChainParameters, Settings}
import scorex.transaction._

/**
  * Waves Transaction Module
  */
class WavesTransactionModule(chainParams: ChainParameters)(implicit override val settings: Settings,
                                                           application: RunnableApplication)
  extends SimpleTransactionModule(chainParams) {

  override def genesisData: Seq[Transaction] = chainParams.genesisTxs

}
