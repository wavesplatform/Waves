package com.wavesplatform

import com.wavesplatform.block.Block
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.test.DomainPresets
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.utils.EthConverters.*
import com.wavesplatform.transaction.{EthTxGenerator, TxHelpers, TxVersion}

/** A chain for export/import tests: legacy blocks first, then protobuf blocks holding legacy-bytes, protobuf and Ethereum transactions, plus
  * empty blocks.
  */
trait MixedChain { self: WithDomain =>
  private val ProtoBlocksFrom = 4

  val mixedChainSettings: WavesSettings = DomainPresets.RideV6.setFeaturesHeight(
    BlockchainFeatures.BlockV5          -> ProtoBlocksFrom,
    BlockchainFeatures.SynchronousCalls -> ProtoBlocksFrom,
    BlockchainFeatures.RideV6           -> ProtoBlocksFrom,
    BlockchainFeatures.BlockReward      -> ProtoBlocksFrom
  )

  def withMixedChain(f: Domain => Unit): Unit =
    withDomain(
      mixedChainSettings,
      Seq(AddrWithBalance(TxHelpers.defaultAddress), AddrWithBalance(TxHelpers.defaultSigner.toEthWavesAddress))
    ) { d =>
      d.appendBlock(Block.NgBlockVersion, TxHelpers.transfer(version = TxVersion.V1), TxHelpers.transfer(version = TxVersion.V2))
      d.appendBlock(Block.NgBlockVersion, TxHelpers.transfer(version = TxVersion.V2))
      d.appendBlock(
        Block.ProtoBlockVersion,
        TxHelpers.transfer(version = TxVersion.V2),
        TxHelpers.transfer(version = TxVersion.V3),
        EthTxGenerator.generateEthTransfer(TxHelpers.defaultEthSigner, TxHelpers.secondAddress, 1, Waves)
      )
      d.appendBlock(Block.ProtoBlockVersion)
      d.appendBlock(Block.ProtoBlockVersion, TxHelpers.transfer(version = TxVersion.V3))
      d.appendBlock(Block.ProtoBlockVersion)
      f(d)
    }
}
