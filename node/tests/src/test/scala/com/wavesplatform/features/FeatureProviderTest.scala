package com.wavesplatform.features

import com.wavesplatform.block.Block
import com.wavesplatform.settings.{BlockchainSettings, FunctionalitySettings, GenesisSettings, RewardsSettings}
import com.wavesplatform.state.{Blockchain, Height}
import com.wavesplatform.test.FlatSpec
import com.wavesplatform.utils.EmptyBlockchain
import org.scalacheck.Gen

class FeatureProviderTest extends FlatSpec {
  "blockVersionAt" should "return valid version" in {
    val fs                 = FunctionalitySettings.MAINNET
    val v3ActivationHeight = Height(fs.blockVersion3AfterHeight)
    val v4ActivationHeight = Height(1740000)
    val v5ActivationHeight = Height(2000000)

    val genesisAt = 1
    val plainAt   = (2 to fs.blockVersion3AfterHeight + 1).toSet
    val ngAt      = (v3ActivationHeight.toInt + 2 to v4ActivationHeight.toInt).toSet
    val rewardAt  = (v4ActivationHeight.toInt + 1 until v5ActivationHeight.toInt).toSet

    val features = Map(
      BlockchainFeatures.BlockReward.id -> v4ActivationHeight,
      BlockchainFeatures.BlockV5.id     -> v5ActivationHeight
    )

    val blockchain = new EmptyBlockchain {
      override def height: Int                           = 1
      override def activatedFeatures: Map[Short, Height] = features
      override lazy val settings: BlockchainSettings     = BlockchainSettings('W', fs, GenesisSettings.MAINNET, RewardsSettings.MAINNET)
    }

    forAll(Gen.choose(1, v5ActivationHeight.toInt * 2)) { h =>
      if (h == genesisAt) blockchain.blockVersionAt(h) shouldBe Block.GenesisBlockVersion
      else if (plainAt contains h) blockchain.blockVersionAt(h) shouldBe Block.PlainBlockVersion
      else if (ngAt contains h) blockchain.blockVersionAt(h) shouldBe Block.NgBlockVersion
      else if (rewardAt contains h) blockchain.blockVersionAt(h) shouldBe Block.RewardBlockVersion
      else blockchain.blockVersionAt(h) shouldBe Block.ProtoBlockVersion
    }

    blockchain.blockVersionAt(v3ActivationHeight.toInt) shouldBe Block.PlainBlockVersion
    blockchain.blockVersionAt(v3ActivationHeight.toInt + 1) shouldBe Block.PlainBlockVersion
    blockchain.blockVersionAt(v3ActivationHeight.toInt + 2) shouldBe Block.NgBlockVersion

    blockchain.blockVersionAt(v4ActivationHeight.toInt) shouldBe Block.NgBlockVersion
    blockchain.blockVersionAt(v4ActivationHeight.toInt + 1) shouldBe Block.RewardBlockVersion

    blockchain.blockVersionAt(v5ActivationHeight.toInt) shouldBe Block.ProtoBlockVersion
  }
}
