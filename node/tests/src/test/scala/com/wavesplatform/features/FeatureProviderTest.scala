package com.wavesplatform.features

import com.wavesplatform.block.Block
import com.wavesplatform.settings.{BlockchainSettings, FunctionalitySettings, GenesisSettings, RewardsSettings}
import com.wavesplatform.state.Blockchain
import com.wavesplatform.test.FlatSpec
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory

class FeatureProviderTest extends FlatSpec with MockFactory {
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

    val blockchain = mock[Blockchain]
    (() => blockchain.height).expects().anyNumberOfTimes().returning(1)
    (() => blockchain.activatedFeatures).expects().anyNumberOfTimes().returning(features)
    (() => blockchain.settings).expects().anyNumberOfTimes().returning(BlockchainSettings('W', fs, GenesisSettings.MAINNET, RewardsSettings.MAINNET))

    forAll(Gen.choose(1, v5ActivationHeight * 2)) { h =>
      if (h == genesisAt) blockchain.blockVersionAt(h) shouldBe Block.GenesisBlockVersion
      else if (plainAt contains h) blockchain.blockVersionAt(h) shouldBe Block.PlainBlockVersion
      else if (ngAt contains h) blockchain.blockVersionAt(h) shouldBe Block.NgBlockVersion
      else if (rewardAt contains h) blockchain.blockVersionAt(h) shouldBe Block.RewardBlockVersion
      else blockchain.blockVersionAt(h) shouldBe Block.ProtoBlockVersion
    }

    blockchain.blockVersionAt(v3ActivationHeight) shouldBe Block.PlainBlockVersion
    blockchain.blockVersionAt(v3ActivationHeight + 1) shouldBe Block.PlainBlockVersion
    blockchain.blockVersionAt(v3ActivationHeight + 2) shouldBe Block.NgBlockVersion

    blockchain.blockVersionAt(v4ActivationHeight) shouldBe Block.NgBlockVersion
    blockchain.blockVersionAt(v4ActivationHeight + 1) shouldBe Block.RewardBlockVersion

    blockchain.blockVersionAt(v5ActivationHeight) shouldBe Block.ProtoBlockVersion
  }
}
