package com.wavesplatform.transaction

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.transaction.Asset.IssuedAsset
import monix.eval.Coeval
import play.api.libs.json.JsObject

trait Transaction {
  val id: Coeval[ByteStr]

  def typeId: Byte = builder.typeId
  def builder: TransactionParser
  def assetFee: (Asset, Long)
  def timestamp: Long
  def chainId: Byte

  def bytesSize: Int         = bytes().length
  val protoSize: Coeval[Int] = Coeval(PBTransactions.protobuf(this).serializedSize)
  val bytes: Coeval[Array[Byte]]
  val json: Coeval[JsObject]

  override def toString: String = json().toString

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
}
