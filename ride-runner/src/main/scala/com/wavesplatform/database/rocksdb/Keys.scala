package com.wavesplatform.database.rocksdb

import com.wavesplatform.state.*

case class CurrentBalance(balance: Long, height: Height, prevHeight: Height)
object CurrentBalance {
  val Unavailable: CurrentBalance = CurrentBalance(0L, Height(0), Height(0))
}

case class BalanceNode(balance: Long, prevHeight: Height)
object BalanceNode {
  val Empty: BalanceNode = BalanceNode(0, Height(0))
}

case class CurrentVolumeAndFee(volume: Long, fee: Long, height: Height, prevHeight: Height)
object CurrentVolumeAndFee {
  val Unavailable: CurrentVolumeAndFee = CurrentVolumeAndFee(0, 0, Height(0), Height(0))
}

case class VolumeAndFeeNode(volume: Long, fee: Long, prevHeight: Height)
object VolumeAndFeeNode {
  val Empty: VolumeAndFeeNode = VolumeAndFeeNode(0, 0, Height(0))
}

case class CurrentLeaseBalance(in: Long, out: Long, height: Height, prevHeight: Height)
object CurrentLeaseBalance {
  val Unavailable: CurrentLeaseBalance = CurrentLeaseBalance(0, 0, Height(0), Height(0))
}

case class LeaseBalanceNode(in: Long, out: Long, prevHeight: Height)
object LeaseBalanceNode {
  val Empty: LeaseBalanceNode = LeaseBalanceNode(0, 0, Height(0))
}

case class CurrentData(entry: DataEntry[?], height: Height, prevHeight: Height)
object CurrentData {
  def empty(key: String): CurrentData = CurrentData(EmptyDataEntry(key), Height(0), Height(0))
}

case class DataNode(entry: DataEntry[?], prevHeight: Height)
object DataNode {
  def empty(key: String): DataNode = DataNode(EmptyDataEntry(key), Height(0))
}
