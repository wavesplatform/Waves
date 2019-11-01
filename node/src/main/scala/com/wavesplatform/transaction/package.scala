package com.wavesplatform

import cats.data.ValidatedNel
import com.wavesplatform.account.PrivateKey
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.sign.TxSigner
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.utils.base58Length

package object transaction {
  val AssetIdLength: Int       = com.wavesplatform.crypto.DigestSize
  val AssetIdStringLength: Int = base58Length(AssetIdLength)

  type DiscardedTransactions = Seq[Transaction]
  type DiscardedBlocks       = Seq[Block]
  type DiscardedMicroBlocks  = Seq[MicroBlock]
  type AuthorizedTransaction = Authorized with Transaction

  type TxType = Byte

  type TxVersion = Byte
  object TxVersion {
    val V1: TxVersion = TxVersion.V1
    val V2: TxVersion = TxVersion.V2
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
