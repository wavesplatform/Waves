package com.wavesplatform.it.sync

import com.typesafe.config.Config
import com.wavesplatform.it.ReportingTestName
import com.wavesplatform.it.sync.activation.ActivationStatusRequest
import com.wavesplatform.it.transactions.NodesFromDocker
import org.scalatest.{CancelAfterFailure, FreeSpec, Matchers, OptionValues}

// todo: (NODE-1927) Integration test
class BlockV5TestSuite
    extends FreeSpec
    with Matchers
    with CancelAfterFailure
    with NodesFromDocker
    with ActivationStatusRequest
    with ReportingTestName
    with OptionValues {
  import BlockV5TestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  "block v5 appears" - {
    "when feature activation happened" in {}
  }
}

object BlockV5TestSuite {
  import com.wavesplatform.it.NodeConfigs.Default

  val Config: Config = Default.head

  val Configs: Seq[Config] = Seq(
    Config.withFallback(Default.head)
  )
}
