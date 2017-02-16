package com.wavesplatform.settings

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._

import scala.concurrent.duration.FiniteDuration

case class MinerSettings(enable: Boolean,
                         offline: Boolean,
                         quorum: Int,
                         generationDelay: FiniteDuration,
                         intervalAfterLastBlockThenGenerationIsAllowed: FiniteDuration,
                         tfLikeScheduling: Boolean)

object MinerSettings {
  val configPath: String = "waves.miner"

  def fromConfig(config: Config): MinerSettings = {
    val enable = config.as[Boolean](s"$configPath.enable")
    val offline = config.as[Boolean](s"$configPath.offline")
    val quorum = config.as[Int](s"$configPath.quorum")
    val generationDelay = config.as[FiniteDuration](s"$configPath.generation-delay")
    val lastBlockInterval = config.as[FiniteDuration](s"$configPath.interval-after-last-block-then-generation-is-allowed")
    val tfLikeScheduling = config.as[Boolean](s"$configPath.tf-like-scheduling")

    MinerSettings(enable, offline, quorum, generationDelay, lastBlockInterval, tfLikeScheduling)
  }
}