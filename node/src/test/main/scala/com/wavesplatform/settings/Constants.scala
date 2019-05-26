package com.wavesplatform.settings

import com.wavesplatform.Version
import com.wavesplatform.transaction.TransactionParsers
import com.wavesplatform.utils.ScorexLogging

/**
  * System constants here.
  */
object Constants extends ScorexLogging {
  val ApplicationName = "waves"
  val AgentName       = s"Waves v${Version.VersionString}"

  val UnitsInWave = 100000000L
  val TotalWaves  = 100000000L

  lazy val TransactionNames: Map[Byte, String] =
    TransactionParsers.all.map {
      case ((typeId, _), builder) =>
        val txName =
          builder.getClass.getSimpleName.init
            .replace("V1", "")
            .replace("V2", "")

        typeId -> txName
    }
}
