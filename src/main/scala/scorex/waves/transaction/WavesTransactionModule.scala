package scorex.waves.transaction

import com.wavesplatform.settings.WavesSettings
import scorex.app.RunnableApplication
import scorex.block.BlockField
import scorex.settings.ChainParameters
import scorex.transaction._

/**
  * Waves Transaction Module
  */
class WavesTransactionModule(chainParams: ChainParameters)(implicit override val settings: WavesSettings,
                                                           application: RunnableApplication)
  extends SimpleTransactionModule(chainParams) {

  override def genesisData: Seq[Transaction] = chainParams.genesisTxs

}
