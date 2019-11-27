package com.wavesplatform.transaction

import com.wavesplatform.transaction.validation.TxValidator

import scala.reflect.ClassTag
import scala.util.Try

trait TransactionParser {
  type TransactionT <: Transaction

  def classTag: ClassTag[TransactionT]

  def typeId: TxType

  def supportedVersions: Set[TxVersion]

  def parseBytes(bytes: Array[Byte]): Try[TransactionT]
  def validator: TxValidator[TransactionT]
}
