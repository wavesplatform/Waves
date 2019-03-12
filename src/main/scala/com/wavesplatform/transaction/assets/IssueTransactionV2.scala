package com.wavesplatform.transaction.assets

import cats.implicits._
import com.google.common.primitives.Bytes
import com.wavesplatform.account.{AddressScheme, PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.description._
import com.wavesplatform.transaction.smart.script.Script
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

case class IssueTransactionV2 private (chainId: Byte,
                                       sender: PublicKeyAccount,
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
        Deser.serializeOption(script)(s => s.bytes().arr)
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
             sender: PublicKeyAccount,
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
    } yield IssueTransactionV2(chainId, sender, name, description, quantity, decimals, reissuable, script, fee, timestamp, proofs)
  }

  def signed(chainId: Byte,
             sender: PublicKeyAccount,
             name: Array[Byte],
             description: Array[Byte],
             quantity: Long,
             decimals: Byte,
             reissuable: Boolean,
             script: Option[Script],
             fee: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    for {
      unverified <- create(chainId, sender, name, description, quantity, decimals, reissuable, script, fee, timestamp, Proofs.empty)
      proofs     <- Proofs.create(Seq(ByteStr(crypto.sign(signer, unverified.bodyBytes()))))
    } yield unverified.copy(proofs = proofs)
  }

  def selfSigned(chainId: Byte,
                 sender: PrivateKeyAccount,
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
      PublicKeyAccountBytes(tailIndex(2), "Sender's public key"),
      BytesArrayUndefinedLength(tailIndex(3), "Name"),
      BytesArrayUndefinedLength(tailIndex(4), "Description"),
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
