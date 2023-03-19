package com.wavesplatform.riderunner.input

import com.wavesplatform.account.PublicKey
import com.wavesplatform.account.PublicKeys.EmptyPublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.script.Script
import com.wavesplatform.state.*
import com.wavesplatform.state.InvokeScriptResult.DataEntry
import com.wavesplatform.transaction.Asset

/** @param data
  *   Some(Map.empty) means the data was here
  */
case class RunnerAccountState(
    scriptInfo: Option[RunnerScriptInfo] = None,
    data: Option[Map[String, RunnerDataEntry]] = None,
    balance: Map[Asset, Long] = Map.empty,
    leasing: Option[RunnerLeaseBalance] = None,
    generatingBalance: Option[Long] = None,
    aliases: List[String] = Nil
)

case class RunnerScriptInfo(
    publicKey: PublicKey = EmptyPublicKey,
    script: Script
)

sealed trait RunnerDataEntry {
  def toDataEntry(key: String): DataEntry
}

// Note, play-json can't find descendants in the companion object

case class BinaryRunnerDataEntry(value: ByteStr) extends RunnerDataEntry {
  override def toDataEntry(key: String): DataEntry = BinaryDataEntry(key, value)
}

case class BooleanRunnerDataEntry(value: Boolean) extends RunnerDataEntry {
  override def toDataEntry(key: String): DataEntry = BooleanDataEntry(key, value)
}

case class IntegerRunnerDataEntry(value: Long) extends RunnerDataEntry {
  override def toDataEntry(key: String): DataEntry = IntegerDataEntry(key, value)
}

case class StringRunnerDataEntry(value: String) extends RunnerDataEntry {
  override def toDataEntry(key: String): DataEntry = StringDataEntry(key, value)
}
