package com.wavesplatform.it

import com.typesafe.config.ConfigFactory.{defaultApplication, defaultReference}
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.db.openDB
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
    val settings         = WavesSettings.fromConfig(sharedConfig)
    val genesisBlock     = Block.genesis(settings.blockchainSettings.genesisSettings).explicitGet()
    val db               = openDB("/tmp/tmp-db")
    val time             = new NTP("ntp.pool.org")
    val portfolioChanges = Observer.empty(UncaughtExceptionReporter.LogExceptionsToStandardErr)
    val bu               = StorageFactory(settings, db, time, portfolioChanges)
    val pos              = new PoSSelector(bu, settings.blockchainSettings, settings.synchronizationSettings)
    bu.processBlock(genesisBlock)

    try {
      NodeConfigs.Default.map(_.withFallback(sharedConfig)).collect {
        case cfg if cfg.as[Boolean]("waves.miner.enable") =>
          val account   = PublicKeyAccount(cfg.as[ByteStr]("public-key").arr)
          val address   = account.toAddress
          val balance   = bu.balance(address, Waves)
          val consensus = genesisBlock.consensusData
          val timeDelay = pos
            .getValidBlockDelay(bu.height, account.publicKey, consensus.baseTarget, balance)
            .explicitGet()

          f"$address: ${timeDelay * 1e-3}%10.3f s"
      }
    } finally time.close()
  }
}
