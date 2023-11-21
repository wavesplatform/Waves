package com.wavesplatform.features

import com.wavesplatform.block.Block
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.test.{FlatSpec, SharedDomain}
import org.scalacheck.Gen

class FeatureProviderTest extends FlatSpec with SharedDomain {
  "blockVersionAt" should "return valid version" in {
    val fs                 = FunctionalitySettings.MAINNET
    val v3ActivationHeight = fs.blockVersion3AfterHeight
    val v4ActivationHeight = 1740000
    val v5ActivationHeight = 2000000

    val genesisAt = 1
    val plainAt   = (2 to fs.blockVersion3AfterHeight + 1).toSet
    val ngAt      = (v3ActivationHeight + 2 to v4ActivationHeight).toSet
    val rewardAt  = (v4ActivationHeight + 1 until v5ActivationHeight).toSet

    val features = Map(
      BlockchainFeatures.BlockReward.id -> v4ActivationHeight,
      BlockchainFeatures.BlockV5.id     -> v5ActivationHeight
    )

    forAll(Gen.choose(1, v5ActivationHeight * 2)) { h =>
      if (h == genesisAt) domain.blockchain.blockVersionAt(h) shouldBe Block.GenesisBlockVersion
      else if (plainAt contains h) domain.blockchain.blockVersionAt(h) shouldBe Block.PlainBlockVersion
      else if (ngAt contains h) domain.blockchain.blockVersionAt(h) shouldBe Block.NgBlockVersion
      else if (rewardAt contains h) domain.blockchain.blockVersionAt(h) shouldBe Block.RewardBlockVersion
      else domain.blockchain.blockVersionAt(h) shouldBe Block.ProtoBlockVersion
    }

    domain.blockchain.blockVersionAt(v3ActivationHeight) shouldBe Block.PlainBlockVersion
    domain.blockchain.blockVersionAt(v3ActivationHeight + 1) shouldBe Block.PlainBlockVersion
    domain.blockchain.blockVersionAt(v3ActivationHeight + 2) shouldBe Block.NgBlockVersion

    domain.blockchain.blockVersionAt(v4ActivationHeight) shouldBe Block.NgBlockVersion
    domain.blockchain.blockVersionAt(v4ActivationHeight + 1) shouldBe Block.RewardBlockVersion

    domain.blockchain.blockVersionAt(v5ActivationHeight) shouldBe Block.ProtoBlockVersion
  }
}
