package com.wavesplatform.ride.runner.input

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.InvokeScriptResult.DataEntry
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, IntegerDataEntry, StringDataEntry}

sealed trait RideRunnerDataEntry {
  def toDataEntry(key: String): DataEntry
}

case class BinaryRideRunnerDataEntry(value: ByteStr) extends RideRunnerDataEntry {
  override def toDataEntry(key: String): DataEntry = BinaryDataEntry(key, value)
}

case class BooleanRideRunnerDataEntry(value: Boolean) extends RideRunnerDataEntry {
  override def toDataEntry(key: String): DataEntry = BooleanDataEntry(key, value)
}

case class IntegerRideRunnerDataEntry(value: Long) extends RideRunnerDataEntry {
  override def toDataEntry(key: String): DataEntry = IntegerDataEntry(key, value)
}

case class StringRideRunnerDataEntry(value: String) extends RideRunnerDataEntry {
  override def toDataEntry(key: String): DataEntry = StringDataEntry(key, value)
}
