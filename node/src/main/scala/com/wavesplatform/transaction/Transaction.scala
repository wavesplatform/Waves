package com.wavesplatform.transaction

import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

trait Transaction {
  val id: Coeval[ByteStr]

  def typeId: Byte = builder.typeId
  def builder: TransactionParser
  def assetFee: (Asset, Long)
  def timestamp: Long
  def chainByte: Option[Byte] = Some(AddressScheme.current.chainId)

  val bytes: Coeval[Array[Byte]]
  val json: Coeval[JsObject]
  override def toString: String = json().toString
  def toPrettyString: String = json.map(Json.prettyPrint).value

  override def equals(other: Any): Boolean = other match {
    case tx: Transaction => id() == tx.id()
    case _               => false
  }

  override def hashCode(): Int = id().hashCode()

  val bodyBytes: Coeval[Array[Byte]]
  def checkedAssets: Seq[IssuedAsset] = Nil
}

object Transaction {
  type Type = Byte

  val V1: TxVersion = TxVersion.V1
  val V2: TxVersion = TxVersion.V2

  implicit class TransactionExt(tx: Transaction) {
    def feeDiff(): Portfolio = tx.assetFee match {
      case (asset @ IssuedAsset(_), fee) =>
        Portfolio(balance = 0, lease = LeaseBalance.empty, assets = Map(asset -> fee))
      case (Waves, fee) => Portfolio(balance = fee, lease = LeaseBalance.empty, assets = Map.empty)
    }
  }

}
