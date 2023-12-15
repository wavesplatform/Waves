package com.wavesplatform.transaction

sealed trait Versioned {
  val version: TxVersion
}

object Versioned {
  trait ConstV1 extends Versioned {
    val version: TxVersion = TxVersion.V1
  }
  trait ToV2 extends Versioned
  trait ToV3 extends Versioned

  def maxVersion(tx: Versioned): TxVersion =
    tx match {
      case _: ConstV1 => TxVersion.V1
      case _: ToV2    => TxVersion.V2
      case _: ToV3    => TxVersion.V3
    }
}
