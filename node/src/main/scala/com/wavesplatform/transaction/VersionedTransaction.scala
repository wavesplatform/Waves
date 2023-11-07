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
}
