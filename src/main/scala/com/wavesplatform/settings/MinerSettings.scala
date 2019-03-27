package com.wavesplatform.settings

import com.typesafe.config.Config
import com.wavesplatform.mining.Miner
import net.ceedubs.ficus.readers.ValueReader

import scala.concurrent.duration.FiniteDuration

case class MinerSettings(enable: Boolean,
                         quorum: Int,
                         intervalAfterLastBlockThenGenerationIsAllowed: FiniteDuration,
                         noQuorumMiningDelay: FiniteDuration,
                         microBlockInterval: FiniteDuration,
                         minimalBlockGenerationOffset: FiniteDuration,
                         maxTransactionsInKeyBlock: Int,
                         maxTransactionsInMicroBlock: Int,
                         minMicroBlockAge: FiniteDuration) {
  require(maxTransactionsInMicroBlock <= Miner.MaxTransactionsPerMicroblock)
}

object MinerSettings {
  implicit val minerSettingsValueReader: ValueReader[MinerSettings] =
    (cfg, path) => fromConfig(cfg.getConfig(path))

  def fromConfig(config: Config): MinerSettings = {
    import net.ceedubs.ficus.Ficus._

    import scala.concurrent.duration._

    val enable                      = config.as[Boolean]("enable")
    val quorum                      = config.as[Int]("quorum")
    val microBlockInterval          = config.as[FiniteDuration]("micro-block-interval")
    val noQuorumMiningDelay         = config.as[FiniteDuration]("no-quorum-mining-delay")
    val offset                      = config.as[Option[FiniteDuration]]("minimal-block-generation-offset").getOrElse(0.millis)
    val blockInterval               = config.as[FiniteDuration]("interval-after-last-block-then-generation-is-allowed")
    val minMicroBlockAge            = config.as[FiniteDuration]("min-micro-block-age")
    val maxTransactionsInKeyBlock   = config.as[Int]("max-transactions-in-key-block")
    val maxTransactionsInMicroBlock = config.as[Int]("max-transactions-in-micro-block")

    MinerSettings(
      enable,
      quorum,
      blockInterval,
      noQuorumMiningDelay,
      microBlockInterval,
      offset,
      maxTransactionsInKeyBlock,
      maxTransactionsInMicroBlock,
      minMicroBlockAge
    )
  }
}
