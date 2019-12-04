package com.wavesplatform.settings

import com.wavesplatform.Version
import com.wavesplatform.transaction.TransactionParsers
import com.wavesplatform.transaction.assets.UpdateAssetInfoTransaction
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
    Map(
      (1: Byte) -> "GenesisTransaction",
      (2: Byte) -> "PaymentTransaction",
      (3: Byte) -> "IssueTransaction",
      (4: Byte) -> "TransferTransaction",
      (5: Byte) -> "ReissueTransaction",
      (6: Byte) -> "BurnTransaction",
      (7: Byte) -> "ExchangeTransaction",
      (8: Byte) -> "LeaseTransaction",
      (9: Byte) -> "LeaseCancelTransaction",
      (10: Byte) -> "CreateAliasTransaction",
      (11: Byte) -> "MassTransferTransaction",
      (12: Byte) -> "DataTransaction",
      (13: Byte) -> "SetScriptTransaction",
      (14: Byte) -> "SponsorFeeTransaction",
      (16: Byte) -> "InvokeScriptTransaction",
      (15: Byte) -> "SetAssetScriptTransaction",
      (17: Byte) -> "UpdateAssetInfoTransaction"
    )

  //    TransactionParsers.all.map {
//      case ((typeId, _), builder) =>
//        val txName =
//          builder.getClass.getSimpleName.init
//            .replace("V1", "")
//            .replace("V2", "")
//
//        typeId -> txName
//    }

//  println(TransactionNames)
}
