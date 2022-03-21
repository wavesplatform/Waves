package com.wavesplatform.transaction

import com.google.common.primitives.Bytes
import com.wavesplatform.account.{AddressScheme, Alias, KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.serialization.impl.CreateAliasTxSerializer
import com.wavesplatform.transaction.validation.impl.CreateAliasTxValidator
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.util.Try

final case class CreateAliasTransaction(version: TxVersion,
                                        sender: PublicKey,
                                        aliasName: String,
                                        fee: TxPositiveAmount,
                                        timestamp: TxTimestamp,
                                        proofs: Proofs,
                                        chainId: Byte) extends SigProofsSwitch
  with VersionedTransaction
  with TxWithFee.InWaves
  with LegacyPBSwitch.V3 {

  lazy val alias: Alias = Alias.createWithChainId(aliasName, chainId).explicitGet()

  override def builder: TransactionParser          = CreateAliasTransaction
  override val bodyBytes: Coeval[Array[TxVersion]] = Coeval.evalOnce(CreateAliasTransaction.serializer.bodyBytes(this))
  override val bytes: Coeval[Array[TxVersion]]     = Coeval.evalOnce(CreateAliasTransaction.serializer.toBytes(this))
  override val json: Coeval[JsObject]              = Coeval.evalOnce(CreateAliasTransaction.serializer.toJson(this))

  override val id: Coeval[ByteStr] = Coeval.evalOnce {
    val payload = version match {
      case TxVersion.V1 | TxVersion.V2 => Bytes.concat(Array(builder.typeId), alias.bytes)
      case _                           => bodyBytes()
    }
    ByteStr(crypto.fastHash(payload))
  }
}

object CreateAliasTransaction extends TransactionParser {
  type TransactionT = CreateAliasTransaction

  val supportedVersions: Set[TxVersion] = Set(1, 2, 3)
  val typeId: TxType                    = 10: Byte

  implicit val validator = CreateAliasTxValidator
  val serializer         = CreateAliasTxSerializer

  implicit def sign(tx: CreateAliasTransaction, privateKey: PrivateKey): CreateAliasTransaction =
    tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))

  override def parseBytes(bytes: Array[TxVersion]): Try[CreateAliasTransaction] =
    serializer.parseBytes(bytes)

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
      tx <- CreateAliasTransaction(version, sender, aliasName, fee, timestamp, proofs, chainId).validatedEither
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

  def selfSigned(version: TxVersion, sender: KeyPair, aliasName: String, fee: Long, timestamp: TxTimestamp, chainId: Byte = AddressScheme.current.chainId): Either[ValidationError, TransactionT] =
    signed(version, sender.publicKey, aliasName, fee, timestamp, sender.privateKey, chainId)
}
