package com.wavesplatform.it.sync.matcher.config

import com.typesafe.config.{Config, ConfigFactory}

import scala.util.Random

object MatcherDefaultConfig {

  import ConfigFactory._
  import com.wavesplatform.it.NodeConfigs._

  val ForbiddenAssetId = "FdbnAsset"
  val orderLimit       = 20

  val minerDisabled = parseString("waves.miner.enable = no")
  val matcherConfig = parseString(s"""
                                     |waves.matcher {
                                     |  enable = yes
                                     |  account = 3HmFkAoQRs4Y3PE2uR6ohN7wS4VqPBGKv7k
                                     |  bind-address = "0.0.0.0"
                                     |  order-match-tx-fee = 300000
                                     |  order-cleanup-interval = 20s
                                     |  blacklisted-assets = ["$ForbiddenAssetId"]
                                     |  balance-watching.enable = yes
                                     |  rest-order-limit=$orderLimit
                                     |}""".stripMargin)

  val Configs: Seq[Config] = (Default.last +: Random.shuffle(Default.init).take(3))
    .zip(Seq(matcherConfig, minerDisabled, minerDisabled, empty()))
    .map { case (n, o) => o.withFallback(n) }

}
