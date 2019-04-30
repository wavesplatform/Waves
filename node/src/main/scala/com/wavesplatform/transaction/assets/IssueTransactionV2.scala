package com.wavesplatform.transaction.assets

import cats.implicits._
import com.google.common.primitives.Bytes
import com.wavesplatform.account.{AddressScheme, KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.description._
import com.wavesplatform.transaction.{validation, _}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

case class IssueTransactionV2 private (chainId: Byte,
                                       sender: PublicKey,
                                       name: Array[Byte],
                                       description: Array[Byte],
                                       quantity: Long,
                                       decimals: Byte,
                                       reissuable: Boolean,
                                       script: Option[Script],
                                       fee: Long,
                                       timestamp: Long,
                                       proofs: Proofs)
    extends IssueTransaction
    with FastHashId
    with ChainSpecific {

  override val builder: TransactionParser = IssueTransactionV2

  override val bodyBytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(
      Bytes.concat(
        Array(builder.typeId, version, chainId),
        bytesBase(),
        Deser.serializeOptionOfArray(script)(s => s.bytes().arr)
      )
    )

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))
  override val json: Coeval[JsObject]     = Coeval.evalOnce(issueJson() ++ Json.obj("chainId" -> chainId, "script" -> script.map(_.bytes().base64)))

  override def version: Byte = 2
}

object IssueTransactionV2 extends TransactionParserFor[IssueTransactionV2] with TransactionParser.MultipleVersions {

  override val typeId: Byte                 = IssueTransaction.typeId
  override val supportedVersions: Set[Byte] = Set(2)

  private def currentChainId = AddressScheme.current.chainId

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    byteTailDescription.deserializeFromByteArray(bytes).flatMap { tx =>
      Either
        .cond(tx.chainId == currentChainId, (), GenericError(s"Wrong chainId actual: ${tx.chainId.toInt}, expected: $currentChainId"))
        .flatMap(_ => IssueTransaction.validateIssueParams(tx))
        .map(_ => tx)
        .foldToTry
    }
  }

  def create(chainId: Byte,
             sender: PublicKey,
             name: Array[Byte],
             description: Array[Byte],
             quantity: Long,
             decimals: Byte,
             reissuable: Boolean,
             script: Option[Script],
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, TransactionT] = {
    for {
      _ <- Either.cond(chainId == currentChainId, (), GenericError(s"Wrong chainId actual: ${chainId.toInt}, expected: $currentChainId"))
      _ <- IssueTransaction.validateIssueParams(name, description, quantity, decimals, reissuable, fee)
      _ <- Either.cond(script.forall(_.isInstanceOf[ExprScript]),
                       (),
                       TxValidationError.GenericError(s"Asset can only be assigned with Expression script, not Contract"))
    } yield IssueTransactionV2(chainId, sender, name, description, quantity, decimals, reissuable, script, fee, timestamp, proofs)
  }

  def signed(chainId: Byte,
             sender: PublicKey,
             name: Array[Byte],
             description: Array[Byte],
             quantity: Long,
             decimals: Byte,
             reissuable: Boolean,
             script: Option[Script],
             fee: Long,
             timestamp: Long,
             signer: PrivateKey): Either[ValidationError, TransactionT] = {
    for {
      unverified <- create(chainId, sender, name, description, quantity, decimals, reissuable, script, fee, timestamp, Proofs.empty)
      proofs     <- Proofs.create(Seq(ByteStr(crypto.sign(signer, unverified.bodyBytes()))))
    } yield unverified.copy(proofs = proofs)
  }

  def selfSigned(chainId: Byte,
                 sender: KeyPair,
                 name: Array[Byte],
                 description: Array[Byte],
                 quantity: Long,
                 decimals: Byte,
                 reissuable: Boolean,
                 script: Option[Script],
                 fee: Long,
                 timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(chainId, sender, name, description, quantity, decimals, reissuable, script, fee, timestamp, sender)
  }

  val byteTailDescription: ByteEntity[IssueTransactionV2] = {
    (
      OneByte(tailIndex(1), "Chain ID"),
      PublicKeyBytes(tailIndex(2), "Sender's public key"),
      BytesArrayUndefinedLength(tailIndex(3), "Name", validation.MaxAssetNameLength, validation.MinAssetNameLength),
      BytesArrayUndefinedLength(tailIndex(4), "Description", validation.MaxDescriptionLength),
      LongBytes(tailIndex(5), "Quantity"),
      OneByte(tailIndex(6), "Decimals"),
      BooleanByte(tailIndex(7), "Reissuable flag (1 - True, 0 - False)"),
      LongBytes(tailIndex(8), "Fee"),
      LongBytes(tailIndex(9), "Timestamp"),
      OptionBytes(index = tailIndex(10), name = "Script", nestedByteEntity = ScriptBytes(tailIndex(10), "Script")),
      ProofsBytes(tailIndex(11))
    ) mapN {
      case (chainId, senderPublicKey, name, desc, quantity, decimals, reissuable, fee, timestamp, script, proofs) =>
        IssueTransactionV2(
          chainId = chainId,
          sender = senderPublicKey,
          name = name,
          description = desc,
          quantity = quantity,
          decimals = decimals,
          reissuable = reissuable,
          script = script,
          fee = fee,
          timestamp = timestamp,
          proofs = proofs
        )
    }
  }
}
