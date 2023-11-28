package com.wavesplatform.transaction

sealed trait VersionedTransaction {
  val version: TxVersion
}

object VersionedTransaction {
  trait ConstV1 extends VersionedTransaction {
    val version: TxVersion = TxVersion.V1
  }
  trait ToV2 extends VersionedTransaction
  trait ToV3 extends VersionedTransaction

  def maxVersion(tx: Transaction): TxVersion =
    tx match {
      case _: ConstV1 => TxVersion.V1
      case _: ToV2    => TxVersion.V2
      case _: ToV3    => TxVersion.V3
    }
}
