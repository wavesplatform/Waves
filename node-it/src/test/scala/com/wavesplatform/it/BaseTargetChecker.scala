package com.wavesplatform.it

import com.typesafe.config.ConfigFactory.{defaultApplication, defaultReference}
import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.database.RDB
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.StorageFactory
import com.wavesplatform.settings.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.utils.NTP
import net.ceedubs.ficus.Ficus.*

object BaseTargetChecker {
  def main(args: Array[String]): Unit = {
    val sharedConfig = Docker
      .genesisOverride()
      .withFallback(Docker.configTemplate)
      .withFallback(defaultApplication())
      .withFallback(defaultReference())
      .resolve()

    val settings               = WavesSettings.fromRootConfig(sharedConfig)
    val db                     = RDB.open(settings.dbSettings.copy(directory = "/tmp/tmp-db"))
    val ntpTime                = new NTP("ntp.pool.org")
    val (blockchainUpdater, _) = StorageFactory(settings, db, ntpTime, BlockchainUpdateTriggers.noop)
    val poSSelector            = PoSSelector(blockchainUpdater, settings.synchronizationSettings.maxBaseTarget)

    try {
      val genesisBlock =
        Block
          .genesis(
            settings.blockchainSettings.genesisSettings,
            blockchainUpdater.isFeatureActivated(BlockchainFeatures.RideV6),
            blockchainUpdater.isFeatureActivated(BlockchainFeatures.TransactionStateSnapshot)
          )
          .explicitGet()
      blockchainUpdater.processBlock(genesisBlock, genesisBlock.header.generationSignature, None)

      NodeConfigs.Default.map(_.withFallback(sharedConfig)).collect {
        case cfg if cfg.as[Boolean]("waves.miner.enable") =>
          val account = KeyPair.fromSeed(cfg.getString("account-seed")).explicitGet()
          val address = account.toAddress
          val balance = blockchainUpdater.balance(address, Waves)
          val timeDelay = poSSelector
            .getValidBlockDelay(blockchainUpdater.height, account, genesisBlock.header.baseTarget, balance)
            .explicitGet()

          f"$address: ${timeDelay * 1e-3}%10.3f s"
      }
    } finally ntpTime.close()
  }
}
