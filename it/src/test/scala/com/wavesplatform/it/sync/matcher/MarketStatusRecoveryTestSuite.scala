package com.wavesplatform.it.sync.matcher

import com.typesafe.config.{Config, ConfigFactory}

class MarketStatusRecoveryTestSuite extends MatcherRecoveryTestSuite {
  // To create a snapshot for each event at least for one order book
  protected override def configOverrides: Config =
    ConfigFactory.parseString("waves.matcher.snapshots-interval = 2").withFallback(super.configOverrides)
}
