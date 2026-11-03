package com.wavesplatform.state

import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures.*
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.{TransactionStateSnapshot, WavesSettingsOps}

class StrictEthSignatureChecksSpec extends FreeSpec with WithDomain {
  "strict Ethereum signature checks" - {
    "are off before any of their features" in withDomain(TransactionStateSnapshot) { d =>
      d.blockchain.strictEthSignatureChecks shouldBe false
    }

    Seq(DeterministicFinality, AdjustedBlockRewardDistribution, FinalityImprovements).foreach { feature =>
      s"are on with ${feature.description} alone" in withDomain(TransactionStateSnapshot.addFeatures(feature)) { d =>
        d.blockchain.strictEthSignatureChecks shouldBe true
      }
    }
  }
}
