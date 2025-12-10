package com.wavesplatform.crypto.bls

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.TxValidationError.GenericError

sealed abstract class BlsSignature(val byteStr: ByteStr) {
  def arr: Array[Byte]          = byteStr.arr
  def base58: String            = byteStr.toString
  override def toString: String = byteStr.toString
}

object BlsSignature {
  val SizeInBytes = 96

  object Empty extends BlsSignature(ByteStr.empty) {
    override def toString: String = "empty"
  }

  case class NonEmpty private (override val byteStr: ByteStr) extends BlsSignature(byteStr) {
    def verifyAgg(message: Array[Byte], blsPks: Iterable[BlsPublicKey]): Either[String, Boolean] =
      BlsUtils.verifyAgg(byteStr.arr, message, blsPks.map(_.arr))
  }

  object NonEmpty {
    // TODO: check size and add def unsafe for append
    def apply(arr: Array[Byte]): NonEmpty               = new NonEmpty(ByteStr(arr))
    def apply(byteStr: ByteStr): NonEmpty               = new NonEmpty(byteStr)
    private[bls] def unsafe(byteStr: ByteStr): NonEmpty = NonEmpty(byteStr)
  }

  def apply(arr: Array[Byte]): Either[ValidationError, NonEmpty] = apply(ByteStr(arr))
  def apply(byteStr: ByteStr): Either[ValidationError, NonEmpty] = Either.cond(
    byteStr.arr.length == SizeInBytes,
    NonEmpty.unsafe(byteStr),
    GenericError(s"Unexpected BLS signature length: ${byteStr.arr.length}, expected: $SizeInBytes")
  )

  def mayBeEmpty(arr: Array[Byte]): Either[ValidationError, BlsSignature] = mayBeEmpty(ByteStr(arr))
  def mayBeEmpty(byteStr: ByteStr): Either[ValidationError, BlsSignature] =
    if (byteStr.isEmpty) Right(BlsSignature.Empty)
    else
      Either.cond(
        byteStr.arr.length == SizeInBytes,
        NonEmpty.unsafe(byteStr),
        GenericError(s"Unexpected BLS signature length: ${byteStr.arr.length}, expected: $SizeInBytes")
      )

  extension (self: BlsSignature) {
    def isDefined: Boolean = self != Empty

    def append(other: BlsSignature.NonEmpty): BlsSignature.NonEmpty = self match {
      case Empty          => other
      case self: NonEmpty => NonEmpty(ByteStr(BlsUtils.aggSign(self.arr, other.arr)))
    }
  }
}
