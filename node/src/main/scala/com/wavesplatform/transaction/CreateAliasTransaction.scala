package com.wavesplatform.transaction

import com.google.common.primitives.Bytes
import com.wavesplatform.account.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.serialization.impl.CreateAliasTxSerializer
import com.wavesplatform.transaction.validation.TxValidator
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
) extends Transaction(TransactionType.CreateAlias),
      ProvenTransaction,
      HasSignature,
      Versioned.ToV3,
      TxWithFee.InWaves,
      PBSince.V3 {

  type T = CreateAliasTransaction

  lazy val alias: Alias = Alias.createWithChainId(aliasName, chainId, Some(chainId)).explicitGet()

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(CreateAliasTxSerializer.bodyBytes(this))
  override val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(CreateAliasTxSerializer.toBytes(this))
  override val json: Coeval[JsObject]         = Coeval.evalOnce(CreateAliasTxSerializer.toJson(this))

  override def addProof(proof: ByteStr): CreateAliasTransaction = copy(proofs = proofs.add(proof))

  override val id: Coeval[ByteStr] = Coeval.evalOnce {
    ByteStr(crypto.fastHash(version match {
      case TxVersion.V1 | TxVersion.V2 => Bytes.concat(Array(tpe.id.toByte), alias.bytes)
      case _                           => bodyBytes()
    }))
  }
}

object CreateAliasTransaction extends TransactionParser {
  type TransactionT = CreateAliasTransaction

  val typeId: TxType = 10: Byte

  implicit val validator: TxValidator[CreateAliasTransaction] = CreateAliasTxValidator

  override def parseBytes(bytes: Array[TxVersion]): Try[CreateAliasTransaction] =
    CreateAliasTxSerializer.parseBytes(bytes)

  def create(
      version: TxVersion,
      sender: PublicKey,
      aliasName: String,
      fee: Long,
      timestamp: TxTimestamp,
      proofs: Proofs = Proofs.empty,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, TransactionT] = {
    for {
      fee <- TxPositiveAmount(fee)(TxValidationError.InsufficientFee)
      tx  <- CreateAliasTransaction(version, sender, aliasName, fee, timestamp, proofs, chainId).validatedEither
    } yield tx
  }
}
