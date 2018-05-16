package com.wavesplatform.it

import java.time.Instant

import com.typesafe.config.ConfigFactory.{defaultApplication, defaultReference}
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.db.openDB
import com.wavesplatform.history.StorageFactory
import com.wavesplatform.settings._
import com.wavesplatform.state.{ByteStr, EitherExt2}
import net.ceedubs.ficus.Ficus._
import scorex.account.PublicKeyAccount
import scorex.block.Block
import scorex.utils.NTP

object BaseTargetChecker {
  def main(args: Array[String]): Unit = {
    val docker = Docker(getClass)
    val sharedConfig = docker.genesisOverride
      .withFallback(docker.configTemplate)
      .withFallback(defaultApplication())
      .withFallback(defaultReference())
      .resolve()
    val settings     = WavesSettings.fromConfig(sharedConfig)
    val genesisBlock = Block.genesis(settings.blockchainSettings.genesisSettings).explicitGet()
    val db           = openDB("/tmp/tmp-db", 1024)
    val bu           = StorageFactory(settings, db, NTP)
    val pos          = new PoSSelector(bu)
    bu.processBlock(genesisBlock)

    println(s"Genesis TS = ${Instant.ofEpochMilli(genesisBlock.timestamp)}")

    val m = NodeConfigs.Default.map(_.withFallback(sharedConfig)).collect {
      case cfg if cfg.as[Boolean]("waves.miner.enable") =>
        val account   = PublicKeyAccount(cfg.as[ByteStr]("public-key").arr)
        val address   = account.toAddress
        val balance   = bu.balance(address, None)
        val consensus = genesisBlock.consensusData
        val timeDelay = pos.validBlockDelay(consensus.generationSignature.arr, account.publicKey, consensus.baseTarget, balance)

        f"$address: ${timeDelay * 1e-3}%10.3f s"
    }

    docker.close()

    println(m.mkString("\n"))
  }
}
