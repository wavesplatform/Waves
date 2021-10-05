package com.wavesplatform

import cats.data.ValidatedNel
import com.wavesplatform.account.PrivateKey
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.Diff
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.utils.{base58Length, EthEncoding}
import play.api.libs.json.{Format, Reads, Writes}
import supertagged._
import supertagged.postfix._

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
  type TxAmount    = Long
  type TxTimestamp = Long
  type TxByteArray = Array[Byte]

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
