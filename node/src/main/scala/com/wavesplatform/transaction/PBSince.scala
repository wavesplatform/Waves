package com.wavesplatform.transaction

sealed trait PBSince

object PBSince {
  trait V1 extends PBSince
  trait V2 extends PBSince
  trait V3 extends PBSince

  def version(tx: PBSince): TxVersion =
    tx match {
      case _: V1 => TxVersion.V1
      case _: V2 => TxVersion.V2
      case _: V3 => TxVersion.V3
    }

  def affects(tx: PBSince & VersionedTransaction): Boolean =
    tx.version >= version(tx)
}
