package com.wavesplatform.lang.impl

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.traits.DataType
import com.wavesplatform.lang.v1.traits.domain.{BlockHeader, Recipient, Tx}

import scala.scalajs.js.annotation.JSGlobalScope
import scala.scalajs.{js => platform}

@platform.native
@JSGlobalScope
object Environment extends scalajs.js.Object {
  type InputEntity = com.wavesplatform.lang.v1.traits.Environment.InputEntity
  def height: Long             = platform.native
  def chainId: Byte            = platform.native
  def tthis: Recipient.Address = platform.native

  def inputEntity: InputEntity                             = platform.native
  def transactionById(id: Array[Byte]): Option[Tx]         = platform.native
  def transactionHeightById(id: Array[Byte]): Option[Long] = platform.native

  def data(addressBytes: Recipient, key: String, dataType: DataType): Option[Any] = platform.native

  def resolveAddress(name: String): Either[String, Recipient.Address] = platform.native

  def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Either[String, Long] = platform.native
  def transactionParser(bytes: Array[Byte]): Option[Tx]                                               = platform.native
  def blockHeaderParser(bytes: Array[Byte]): Option[BlockHeader]                                      = platform.native
  def calculatePoSDelay(hit: ByteStr, baseTarget: Long, balance: Long): Long                          = platform.native
}
