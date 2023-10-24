package com.wavesplatform.transaction.transfer

import scala.util.{Either, Try}

import cats.instances.list._
import cats.syntax.traverse._
import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError._
import com.wavesplatform.transaction.serialization.impl.MassTransferTxSerializer
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.transaction.validation.impl.MassTransferTxValidator
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

case class MassTransferTransaction(
    version: TxVersion,
    sender: PublicKey,
    assetId: Asset,
    transfers: Seq[ParsedTransfer],
    fee: TxPositiveAmount,
    timestamp: TxTimestamp,
    attachment: ByteStr,
    proofs: Proofs,
    chainId: Byte
) extends Transaction(TransactionType.MassTransfer, assetId match {
      case Waves          => Seq()
      case a: IssuedAsset => Seq(a)
    })
    with ProvenTransaction
    with VersionedTransaction.ToV2
    with TxWithFee.InWaves
    with FastHashId
    with PBSince.V2 {

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(MassTransferTxSerializer.bodyBytes(this))
  override val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(MassTransferTxSerializer.toBytes(this))
  override val json: Coeval[JsObject]         = Coeval.evalOnce(MassTransferTxSerializer.toJson(this))

  def compactJson(recipient: Address, aliases: Set[Alias]): JsObject =
    json() ++ Json.obj(
      "transfers" -> MassTransferTxSerializer.transfersJson(transfers.filter { t =>
        t.address match {
          case a: Address => a == recipient
          case a: Alias   => aliases(a)
        }
      })
    )
}

object MassTransferTransaction extends TransactionParser {
  type TransactionT = MassTransferTransaction

  val MaxTransferCount = 100

  override val typeId: TxType                    = 11: Byte
  override val supportedVersions: Set[TxVersion] = Set(1, 2)

  implicit val validator: TxValidator[MassTransferTransaction] = MassTransferTxValidator

  implicit def sign(tx: MassTransferTransaction, privateKey: PrivateKey): MassTransferTransaction =
    tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))

  override def parseBytes(bytes: Array[Byte]): Try[MassTransferTransaction] =
    MassTransferTxSerializer.parseBytes(bytes)

  case class Transfer(
      recipient: String,
      amount: Long
  )

  object Transfer {
    implicit val jsonFormat = Json.format[Transfer]
  }

  case class ParsedTransfer(address: AddressOrAlias, amount: TxNonNegativeAmount)

  def create(
      version: TxVersion,
      sender: PublicKey,
      assetId: Asset,
      transfers: Seq[ParsedTransfer],
      fee: Long,
      timestamp: TxTimestamp,
      attachment: ByteStr,
      proofs: Proofs,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, MassTransferTransaction] =
    for {
      fee <- TxPositiveAmount(fee)(TxValidationError.InsufficientFee)
      tx  <- MassTransferTransaction(version, sender, assetId, transfers, fee, timestamp, attachment, proofs, chainId).validatedEither
    } yield tx

  def signed(
      version: TxVersion,
      sender: PublicKey,
      assetId: Asset,
      transfers: Seq[ParsedTransfer],
      fee: Long,
      timestamp: TxTimestamp,
      attachment: ByteStr,
      signer: PrivateKey,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, MassTransferTransaction] =
    create(version, sender, assetId, transfers, fee, timestamp, attachment, Proofs.empty, chainId).map(_.signWith(signer))

  def selfSigned(
      version: TxVersion,
      sender: KeyPair,
      assetId: Asset,
      transfers: Seq[ParsedTransfer],
      fee: Long,
      timestamp: TxTimestamp,
      attachment: ByteStr,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, MassTransferTransaction] =
    signed(version, sender.publicKey, assetId, transfers, fee, timestamp, attachment, sender.privateKey, chainId)

  def parseTransfersList(transfers: List[Transfer]): Validation[List[ParsedTransfer]] =
    transfers.traverse {
      case Transfer(recipient, amount) =>
        for {
          addressOrAlias <- AddressOrAlias.fromString(recipient)
          transferAmount <- TxNonNegativeAmount(amount)(NegativeAmount(amount, "asset"))
        } yield {
          ParsedTransfer(addressOrAlias, transferAmount)
        }
    }

}
