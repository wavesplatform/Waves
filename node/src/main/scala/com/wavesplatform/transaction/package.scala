package com.wavesplatform

import cats.data.ValidatedNel
import com.wavesplatform.account.PrivateKey
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.Diff
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.utils.base58Length
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.{Interval, NonNegative, Positive}

package object transaction {
  val AssetIdLength: Int       = com.wavesplatform.crypto.DigestLength
  val AssetIdStringLength: Int = base58Length(AssetIdLength)

  type DiscardedBlocks       = Seq[(Block, ByteStr)]
  type DiscardedMicroBlocks  = Seq[(MicroBlock, Diff)]
  type AuthorizedTransaction = Authorized with Transaction

  type TxType = Byte

  type TxVersion = Byte
  object TxVersion {
    val V1: TxVersion = 1.toByte
    val V2: TxVersion = 2.toByte
    val V3: TxVersion = 3.toByte
  }
  type TxTimestamp = Long
  type TxByteArray = Array[Byte]

  type TxAmount = Long Refined Positive
  object TxAmount extends RefinedTypeOps[TxAmount, Long]

  type TxQuantity = Long Refined NonNegative
  object TxQuantity extends RefinedTypeOps[TxQuantity, Long]

  type TxDecimals = Byte Refined Interval.Closed[0, IssueTransaction.MaxAssetDecimals.type ]
  object TxDecimals extends RefinedTypeOps[TxDecimals, Byte] {
    val errMsg = s"decimals should be in interval [0; ${IssueTransaction.MaxAssetDecimals}]"
  }

  type TxOrderPrice = Long Refined Positive
  object TxOrderPrice extends RefinedTypeOps[TxOrderPrice, Long] {
    val errMsg = "price should be > 0"
  }

  type TxExchangeAmount = Long Refined Interval.OpenClosed[0, Order.MaxAmount.type ]
  object TxExchangeAmount extends RefinedTypeOps[TxExchangeAmount, Long] {
    val errMsg = s"amount should be in interval (0; ${Order.MaxAmount}]"
  }

  type TxExchangePrice = Long Refined Interval.OpenClosed[0, Order.MaxAmount.type ]
  object TxExchangePrice extends RefinedTypeOps[TxExchangePrice, Long] {
    val errMsg = s"price should be in interval (0; ${Order.MaxAmount}]"
  }

  type TxMatcherFee = Long Refined Interval.OpenClosed[0, Order.MaxAmount.type ]
  object TxMatcherFee extends RefinedTypeOps[TxMatcherFee, Long] {
    val errMsg = s"matcher fee should be in interval (0; ${Order.MaxAmount}]"
  }

  implicit class TransactionValidationOps[T <: Transaction](val tx: T) extends AnyVal {
    def validatedNel(implicit validator: TxValidator[T]): ValidatedNel[ValidationError, T] = validator.validate(tx)
    def validatedEither(implicit validator: TxValidator[T]): Either[ValidationError, T]    = this.validatedNel.toEither.left.map(_.head)
  }

  implicit class TransactionSignOps[T](val tx: T) extends AnyVal {
    def signWith(privateKey: PrivateKey)(implicit sign: (T, PrivateKey) => T): T = sign(tx, privateKey)
  }
}
