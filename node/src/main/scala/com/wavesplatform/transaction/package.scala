package com.wavesplatform

import cats.data.ValidatedNel
import com.wavesplatform.account.PrivateKey
import com.wavesplatform.block.{Block, BlockSnapshot, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.StateSnapshot
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.utils.{EthEncoding, base58Length}
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.{Interval, NonNegative, Positive}
import play.api.libs.json.*
import supertagged.*
import supertagged.postfix.*

package object transaction {
  val AssetIdLength: Int       = com.wavesplatform.crypto.DigestLength
  val AssetIdStringLength: Int = base58Length(AssetIdLength)

  type DiscardedBlocks       = Seq[(Block, ByteStr, Option[BlockSnapshot])]
  type DiscardedMicroBlocks  = Seq[(MicroBlock, StateSnapshot)]
  type AuthorizedTransaction = Authorized & Transaction

  type TxType = Byte

  type TxVersion = Byte
  object TxVersion {
    val V1: TxVersion = 1.toByte
    val V2: TxVersion = 2.toByte
    val V3: TxVersion = 3.toByte
  }
  type TxTimestamp = Long
  type TxByteArray = Array[Byte]

  type TxPositiveAmount = Long Refined Positive
  object TxPositiveAmount extends RefinedTypeOps[TxPositiveAmount, Long]

  type TxNonNegativeAmount = Long Refined NonNegative
  object TxNonNegativeAmount extends RefinedTypeOps[TxNonNegativeAmount, Long] {
    private val LongStringMaxLength = 20 // Long.MaxValue.toString.length

    implicit val reads: Reads[TxNonNegativeAmount] = Reads { json =>
      val r = json match {
        case JsString(s) =>
          if (s.length > LongStringMaxLength) JsError("error.expected.numberdigitlimit")
          else
            s.toLongOption match {
              case None    => JsError(JsonValidationError("error.expected.numberformatexception"))
              case Some(r) => JsSuccess(r)
            }

        case JsNumber(r) =>
          if (r.isValidLong) JsSuccess(r.toLongExact)
          else JsError(JsonValidationError("error.invalid.long"))

        case _ => JsError(JsonValidationError("error.expected.jsnumberorjsstring"))
      }

      r.flatMap { r =>
        if (r >= 0) JsSuccess(TxNonNegativeAmount.unsafeFrom(r))
        else JsError(JsonValidationError("error.expected.txnonnegativeamount"))
      }
    }
  }

  type TxDecimals = Byte Refined Interval.Closed[0, IssueTransaction.MaxAssetDecimals.type]
  object TxDecimals extends RefinedTypeOps[TxDecimals, Byte] {
    val errMsg = s"decimals should be in interval [0; ${IssueTransaction.MaxAssetDecimals}]"
  }

  type TxOrderPrice = Long Refined Positive
  object TxOrderPrice extends RefinedTypeOps[TxOrderPrice, Long] {
    val errMsg = "price should be > 0"
  }

  type TxExchangeAmount = Long Refined Interval.OpenClosed[0, Order.MaxAmount.type]
  object TxExchangeAmount extends RefinedTypeOps[TxExchangeAmount, Long] {
    val errMsg = s"amount should be in interval (0; ${Order.MaxAmount}]"
  }

  type TxExchangePrice = Long Refined Interval.OpenClosed[0, Order.MaxAmount.type]
  object TxExchangePrice extends RefinedTypeOps[TxExchangePrice, Long] {
    val errMsg = s"price should be in interval (0; ${Order.MaxAmount}]"
  }

  type TxMatcherFee = Long Refined Interval.Open[0, Order.MaxAmount.type]
  object TxMatcherFee extends RefinedTypeOps[TxMatcherFee, Long] {
    val errMsg = s"matcher fee should be in interval (0; ${Order.MaxAmount})"
  }

  implicit class TransactionValidationOps[T <: Transaction](val tx: T) extends AnyVal {
    def validatedNel(implicit validator: TxValidator[T]): ValidatedNel[ValidationError, T] = validator.validate(tx)
    def validatedEither(implicit validator: TxValidator[T]): Either[ValidationError, T]    = this.validatedNel.toEither.left.map(_.head)
  }

  implicit class TransactionSignOps[T](val tx: T) extends AnyVal {
    def signWith(privateKey: PrivateKey)(implicit sign: (T, PrivateKey) => T): T = sign(tx, privateKey)
  }

  object ERC20Address extends TaggedType[ByteStr] {
    def apply(bs: ByteStr): ERC20Address = {
      require(bs.arr.length == 20, "ERC20 token address length must be 20 bytes")
      bs @@ this
    }

    def apply(ia: IssuedAsset): ERC20Address = apply(ia.id.take(20))

    implicit val jsonFormat: Format[ERC20Address] = Format(
      implicitly[Reads[String]].map(str => ERC20Address(ByteStr(EthEncoding.toBytes(str)))),
      implicitly[Writes[String]].contramap((addr: ERC20Address) => EthEncoding.toHexString(addr.arr))
    )
  }
  type ERC20Address = ERC20Address.Type
}
