package com.wavesplatform.transaction.serialization

import com.wavesplatform.transaction.Transaction
import play.api.libs.json.JsObject

import scala.util.Try

trait TxSerializer[T <: Transaction] {
  def toJson(tx: T): JsObject
  def bodyBytes(tx: T): Array[Byte]
  def toBytes(tx: T): Array[Byte]
  def parseBytes(bytes: Array[Byte]): Try[T]
}

object TxSerializer {
  def apply[T <: Transaction : TxSerializer]: TxSerializer[T] = implicitly[TxSerializer[T]]
}
