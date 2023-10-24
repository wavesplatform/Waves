package com.wavesplatform.transaction

trait VersionedTransaction {
  val version: TxVersion
  val maxVersion: TxVersion
}

object VersionedTransaction {
  trait ConstV1 extends VersionedTransaction {
    val version: TxVersion    = TxVersion.V1
    val maxVersion: TxVersion = TxVersion.V1
  }
  trait ToV2 extends VersionedTransaction {
    val maxVersion: TxVersion = TxVersion.V2
  }
  trait ToV3 extends VersionedTransaction {
    val maxVersion: TxVersion = TxVersion.V3
  }
}
