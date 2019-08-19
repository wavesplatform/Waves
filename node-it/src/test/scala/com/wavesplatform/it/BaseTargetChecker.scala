package com.wavesplatform.it

import com.typesafe.config.ConfigFactory.{defaultApplication, defaultReference}
import com.wavesplatform.account.PublicKey
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.database.openDB
import com.wavesplatform.history.StorageFactory
import com.wavesplatform.settings._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.utils.NTP
import monix.execution.UncaughtExceptionReporter
import monix.reactive.Observer
import net.ceedubs.ficus.Ficus._

object BaseTargetChecker {
  def main(args: Array[String]): Unit = {
    val sharedConfig = Docker.genesisOverride
      .withFallback(Docker.configTemplate)
      .withFallback(defaultApplication())
      .withFallback(defaultReference())
      .resolve()

    val settings          = WavesSettings.fromRootConfig(sharedConfig)
    val db                = openDB("/tmp/tmp-db")
    val ntpTime           = new NTP("ntp.pool.org")
    val portfolioChanges  = Observer.empty(UncaughtExceptionReporter.default)
    val blockchainUpdater = StorageFactory(settings, db, ntpTime, portfolioChanges)
    val poSSelector       = new PoSSelector(blockchainUpdater, settings.blockchainSettings, settings.synchronizationSettings)

    try {
      val genesisBlock = Block.genesis(settings.blockchainSettings.genesisSettings).explicitGet()
      blockchainUpdater.processBlock(genesisBlock)

      NodeConfigs.Default.map(_.withFallback(sharedConfig)).collect {
        case cfg if cfg.as[Boolean]("waves.miner.enable") =>
          val account   = PublicKey(cfg.as[ByteStr]("public-key").arr)
          val address   = account.toAddress
          val balance   = blockchainUpdater.balance(address, Waves)
          val consensus = genesisBlock.consensusData
          val timeDelay = poSSelector
            .getValidBlockDelay(blockchainUpdater.height, account, consensus.baseTarget, balance)
            .explicitGet()

          f"$address: ${timeDelay * 1e-3}%10.3f s"
      }
    } finally ntpTime.close()
  }
}
