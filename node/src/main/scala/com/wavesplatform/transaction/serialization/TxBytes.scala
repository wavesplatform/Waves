package com.wavesplatform.transaction.serialization

import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Transaction
import play.api.libs.json.JsObject

import scala.util.Try

trait TxBytes[T <: Transaction] {
  def bodyBytes(tx: T): Array[Byte]
  def toBytes(tx: T): Array[Byte]
  def parseBytes(bytes: Array[Byte]): Try[T]
}
