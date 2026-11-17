package com.wavesplatform.utils

import com.typesafe.config.ConfigFactory
import com.wavesplatform.test.FreeSpec

class SystemInformationReporterSpec extends FreeSpec {
  private val config = ConfigFactory.parseString(
    """waves.miner {
      |  quorum = 3
      |  private-keys = ["secret"]
      |}
      |""".stripMargin
  )

  "the logged configuration" - {
    "leaves out the miner's private keys" in {
      SystemInformationReporter.configForLogs(config).hasPath("waves.miner.private-keys") shouldBe false
    }

    "keeps the rest of the miner settings" in {
      SystemInformationReporter.configForLogs(config).getInt("waves.miner.quorum") shouldBe 3
    }
  }
}
