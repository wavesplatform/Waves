package com.wavesplatform.settings

import com.wavesplatform.Version
import com.wavesplatform.transaction.TransactionParsers
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.utils.ScorexLogging

/**
  * System constants here.
  */
object Constants extends ScorexLogging {
  val ApplicationName = "waves"
  val AgentName       = s"Waves v${Version.VersionString}"

  val UnitsInWave = 100000000L
  val TotalWaves  = 100000000L

  private[this] def txName(className: String): String =
    className.init
      .replace("V1", "")
      .replace("V2", "")

  // todo: (NODE-1915) eliminate dependency to parsers
  lazy val TransactionNames: Map[Byte, String] =
    TransactionParsers.all.map {
      case ((typeId, _), builder) => typeId -> txName(builder.getClass.getSimpleName)
    } + (TransferTransaction.transactionType.toByte -> txName(TransferTransaction.getClass.getSimpleName))
}
