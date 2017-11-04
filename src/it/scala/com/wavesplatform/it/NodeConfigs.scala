package com.wavesplatform.it

import com.typesafe.config.{Config, ConfigFactory}

import scala.collection.JavaConverters._
import scala.util.Random

object NodeConfigs {

  val default: Seq[Config] = Docker.NodeConfigs.getConfigList("nodes").asScala

  def default(miners: Int, nonMiners: Int = 0): Seq[Config] = {
    val nonGeneratingPeerConfig = ConfigFactory.parseString(
      """
        |waves.miner.enable=no
      """.stripMargin
    )

    val (minerConfigs: Seq[Config], nonMinerConfigs: Seq[Config]) = Random.shuffle(default)
      .take(miners + nonMiners)
      .splitAt(miners)

    minerConfigs ++ nonMinerConfigs.map { orig => nonGeneratingPeerConfig.withFallback(orig) }
  }

}
