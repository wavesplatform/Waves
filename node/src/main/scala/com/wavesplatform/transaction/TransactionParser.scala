package com.wavesplatform.transaction

import scala.reflect.ClassTag
import scala.util.Try

trait TransactionParser {
  type TransactionT <: Transaction

  def classTag: ClassTag[TransactionT]

  def typeId: TxType

  def supportedVersions: Set[TxVersion]

  def parseBytes(bytes: Array[Byte]): Try[TransactionT]
}
