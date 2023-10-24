package com.wavesplatform.transaction

import com.google.common.primitives.Bytes
import com.wavesplatform.account.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.serialization.impl.CreateAliasTxSerializer
import com.wavesplatform.transaction.validation.impl.CreateAliasTxValidator
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.util.Try

final case class CreateAliasTransaction(
    version: TxVersion,
    sender: PublicKey,
    aliasName: String,
    fee: TxPositiveAmount,
    timestamp: TxTimestamp,
    proofs: Proofs,
    chainId: Byte
) extends Transaction(TransactionType.CreateAlias)
    with SigProofsSwitch
    with VersionedTransaction.ToV3
    with TxWithFee.InWaves
    with PBSince.V3 {

  lazy val alias: Alias = Alias.createWithChainId(aliasName, chainId).explicitGet()

  override val bodyBytes: Coeval[Array[TxVersion]] = Coeval.evalOnce(CreateAliasTxSerializer.bodyBytes(this))
  override val bytes: Coeval[Array[TxVersion]]     = Coeval.evalOnce(CreateAliasTxSerializer.toBytes(this))
  override val json: Coeval[JsObject]              = Coeval.evalOnce(CreateAliasTxSerializer.toJson(this))

  override val id: Coeval[ByteStr] = Coeval.evalOnce {
    ByteStr(crypto.fastHash(version match {
      case TxVersion.V1 | TxVersion.V2 => Bytes.concat(Array(tpe.id.toByte), alias.bytes)
      case _                           => bodyBytes()
    }))
  }
}

object CreateAliasTransaction extends TransactionParser {
  type TransactionT = CreateAliasTransaction

  val supportedVersions: Set[TxVersion] = Set(1, 2, 3)
  val typeId: TxType                    = 10: Byte

  implicit val validator = CreateAliasTxValidator

  implicit def sign(tx: CreateAliasTransaction, privateKey: PrivateKey): CreateAliasTransaction =
    tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))

  override def parseBytes(bytes: Array[TxVersion]): Try[CreateAliasTransaction] =
    CreateAliasTxSerializer.parseBytes(bytes)

  def create(
      version: TxVersion,
      sender: PublicKey,
      aliasName: String,
      fee: Long,
      timestamp: TxTimestamp,
      proofs: Proofs,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, TransactionT] = {
    for {
      fee <- TxPositiveAmount(fee)(TxValidationError.InsufficientFee)
      tx  <- CreateAliasTransaction(version, sender, aliasName, fee, timestamp, proofs, chainId).validatedEither
    } yield tx
  }

  def signed(
      version: TxVersion,
      sender: PublicKey,
      alias: String,
      fee: Long,
      timestamp: TxTimestamp,
      signer: PrivateKey,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, TransactionT] =
    create(version, sender, alias, fee, timestamp, Nil, chainId).map(_.signWith(signer))

  def selfSigned(
      version: TxVersion,
      sender: KeyPair,
      aliasName: String,
      fee: Long,
      timestamp: TxTimestamp,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, TransactionT] =
    signed(version, sender.publicKey, aliasName, fee, timestamp, sender.privateKey, chainId)
}
