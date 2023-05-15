package com.wavesplatform.ride.runner.input

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.InvokeScriptResult.DataEntry
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, IntegerDataEntry, StringDataEntry}

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
