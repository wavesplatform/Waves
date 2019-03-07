package com.wavesplatform.transaction

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.serialization.{BytesSerializable, JsonSerializable}
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import monix.eval.Coeval

trait Transaction extends BytesSerializable with JsonSerializable {
  val id: Coeval[ByteStr]

  def builder: TransactionParser
  def assetFee: (Asset, Long)
  def timestamp: Long

  override def toString: String = json().toString()

  override def equals(other: Any): Boolean = other match {
    case tx: Transaction => id() == tx.id()
    case _               => false
  }

  override def hashCode(): Int = id().hashCode()

  val bodyBytes: Coeval[Array[Byte]]
  def checkedAssets(): Seq[Asset] = Seq.empty
}

object Transaction {

  type Type = Byte

  implicit class TransactionExt(tx: Transaction) {
    def feeDiff(): Portfolio = tx.assetFee match {
      case (asset @ IssuedAsset(_), fee) =>
        Portfolio(balance = 0, lease = LeaseBalance.empty, assets = Map(asset -> fee))
      case (Waves, fee) => Portfolio(balance = fee, lease = LeaseBalance.empty, assets = Map.empty)
    }
  }

}
