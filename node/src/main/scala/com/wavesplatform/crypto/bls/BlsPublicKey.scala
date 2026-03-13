package com.wavesplatform.crypto.bls

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.TxValidationError.GenericError

opaque type BlsPublicKey = ByteStr

object BlsPublicKey {
  val SizeInBytes = BlsUtils.PublicKeySizeInBytes

  extension (self: BlsPublicKey) {
    def byteStr: ByteStr = self
    def arr: Array[Byte] = byteStr.arr
    def base58: String   = byteStr.toString

    /** We need this once when adding a new endorser
      */
    def validated: Either[String, Unit] = BlsUtils.validatePublicKey(arr)
  }

  private[bls] def unchecked(byteStr: ByteStr): BlsPublicKey = byteStr

  def apply(arr: Array[Byte]): Either[GenericError, BlsPublicKey] = apply(ByteStr(arr))
  def apply(byteStr: ByteStr): Either[GenericError, BlsPublicKey] = BlsUtils.sanityCheckPublicKey(byteStr.arr) match {
    case Right(_)  => Right(byteStr)
    case Left(err) => Left(GenericError(err))
  }
}
