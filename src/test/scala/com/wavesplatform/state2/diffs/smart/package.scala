package com.wavesplatform.state2.diffs

import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.TypeChecker
import com.wavesplatform.lang.ctx.Context
import com.wavesplatform.settings.FunctionalitySettings
import monix.eval.Coeval
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.smart.ConsensusContext

package object smart {
  val smartEnabledFS: FunctionalitySettings                               = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0))
  val dummyContext: Context                                   = new ConsensusContext(Coeval(???), Coeval(???), null).build()
  val dummyTypeCheckerContext: TypeChecker.TypeCheckerContext = TypeChecker.TypeCheckerContext.fromContext(dummyContext)

}
