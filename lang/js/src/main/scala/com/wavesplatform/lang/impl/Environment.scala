package com.wavesplatform.lang.impl

import com.wavesplatform.lang.traits.{DataType, Transaction}

import scala.scalajs.js.annotation.JSGlobalScope
import scala.scalajs.{js => platform}

@platform.native
@JSGlobalScope
object Environment extends scalajs.js.Object {
  def height: Int       = platform.native
  def networkByte: Byte = platform.native

  def transaction: Transaction                              = platform.native
  def transactionById(id: Array[Byte]): Option[Transaction] = platform.native

  def data(addressBytes: Array[Byte], key: String, dataType: DataType): Option[Any] = platform.native

  def resolveAddress(addressOrAlias: Array[Byte]): Either[String, Array[Byte]] = platform.native

  def accountBalanceOf(addressOrAlias: Array[Byte], assetId: Option[Array[Byte]]): Either[String, Long] = platform.native
}
