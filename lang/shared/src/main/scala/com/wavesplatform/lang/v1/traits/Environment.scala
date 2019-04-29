package com.wavesplatform.lang.v1.traits

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.traits.domain.Tx.ScriptTransfer
import com.wavesplatform.lang.v1.traits.domain._
import shapeless._

object Environment {
  type InputEntity = Tx :+: Ord :+: ScriptTransfer :+: CNil
}

trait Environment {
  def height: Long
  def chainId: Byte
  def inputEntity: Environment.InputEntity
  def tthis: Recipient.Address
  def transactionById(id: Array[Byte]): Option[Tx]
  def transactionParser(bytes: Array[Byte]): Option[Tx]
  def transactionHeightById(id: Array[Byte]): Option[Long]
  def assetInfoById(id: Array[Byte]): Option[ScriptAssetInfo]
  def lastBlockOpt(): Option[BlockInfo]
  def data(addressOrAlias: Recipient, key: String, dataType: DataType): Option[Any]
  def resolveAlias(name: String): Either[String, Recipient.Address]
  def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Either[String, Long]
  def blockHeaderParser(bytes: Array[Byte]): Option[BlockHeader]
  def accountScriptHash(addressOrAlias: Recipient): Option[Array[Byte]]
  def calculatePoSDelay(hit: ByteStr, baseTarget: Long, balance: Long): Long
}
