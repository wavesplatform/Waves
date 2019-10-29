package com.wavesplatform

import cats.data.ValidatedNel
import com.wavesplatform.account.PrivateKey
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.sign.TxSigner
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.utils.base58Length
import io.estatico.newtype.macros.newtype

package object transaction {
  val AssetIdLength: Int       = com.wavesplatform.crypto.DigestSize
  val AssetIdStringLength: Int = base58Length(AssetIdLength)

  type DiscardedTransactions = Seq[Transaction]
  type DiscardedBlocks       = Seq[Block]
  type DiscardedMicroBlocks  = Seq[MicroBlock]
  type AuthorizedTransaction = Authorized with Transaction

  @newtype case class TxType(toByte: Byte)
  object TxType {
    implicit def fromInt(i: Int): Int = TxType(i.toByte)
    implicit def toInt(v: TxType): Int = v.toByte
    implicit def fromByte(i: Byte): TxType = TxType(i)
    implicit def toByte(v: TxType): Byte = v.toByte
  }

  @newtype case class TxVersion(toByte: Byte)
  object TxVersion {
    val V1 = TxVersion(1: Byte)
    val V2 = TxVersion(2: Byte)

    implicit def fromInt(i: Int): Int = TxVersion(i.toByte)
    implicit def toInt(v: TxVersion): Int = v.toByte
    implicit def fromByte(i: Byte): TxVersion = TxVersion(i)
    implicit def toByte(v: TxVersion): Byte = v.toByte
  }
  type TxAmount    = Long
  type TxTimestamp = Long
  type TxByteArray = Array[Byte]

  implicit class TransactionValidationOps[T <: Transaction: TxValidator](tx: T) {
    def validatedNel: ValidatedNel[ValidationError, T] = implicitly[TxValidator[T]].validate(tx)
    def validatedEither: Either[ValidationError, T]    = this.validatedNel.toEither.left.map(_.head)
  }

  implicit class TransactionSignOps[T <: Transaction: TxSigner](tx: T) {
    def signWith(privateKey: PrivateKey): T = implicitly[TxSigner[T]].sign(tx, privateKey)
  }
}
