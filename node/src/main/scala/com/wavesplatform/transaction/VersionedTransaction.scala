package com.wavesplatform.transaction

trait VersionedTransaction {
  def version: TxVersion
}

object VersionedTransaction {
  trait ConstV1 extends VersionedTransaction {
    def version: TxVersion = TxVersion.V1
  }
}