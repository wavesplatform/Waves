package com.wavesplatform.transaction.smart

import cats.implicits._
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxValidationError._
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.description._
import monix.eval.Coeval
import play.api.libs.json.Json

import scala.util.Try

case class SetScriptTransaction private (chainId: Byte, sender: PublicKey, script: Option[Script], fee: Long, timestamp: Long, proofs: Proofs)
    extends ProvenTransaction
    with VersionedTransaction
    with FastHashId {

  override val builder: TransactionParser = SetScriptTransaction

  val bodyBytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(
      Bytes.concat(
        Array(builder.typeId, version, chainId),
        sender,
        Deser.serializeOptionOfArray(script)(s => s.bytes().arr),
        Longs.toByteArray(fee),
        Longs.toByteArray(timestamp)
      )
    )

  override val assetFee: (Asset, Long) = (Waves, fee)
  override val json                    = Coeval.evalOnce(jsonBase() ++ Json.obj("chainId" -> chainId, "version" -> version, "script" -> script.map(_.bytes().base64)))

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))
  override def version: Byte              = 1
}

object SetScriptTransaction extends TransactionParserFor[SetScriptTransaction] with TransactionParser.MultipleVersions {

  override val typeId: Byte                 = 13
  override val supportedVersions: Set[Byte] = Set(1)

  private def chainId: Byte = AddressScheme.current.chainId

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    byteTailDescription.deserializeFromByteArray(bytes).flatMap { tx =>
      Either
        .cond(tx.chainId == chainId, (), GenericError(s"Wrong chainId ${tx.chainId.toInt}"))
        .flatMap(_ => Either.cond(tx.fee > 0, (), InsufficientFee(s"insufficient fee: ${tx.fee}")))
        .map(_ => tx)
        .foldToTry
    }
  }

  def create(sender: PublicKey, script: Option[Script], fee: Long, timestamp: Long, proofs: Proofs): Either[ValidationError, TransactionT] = {
    for {
      _ <- Either.cond(fee > 0, (), InsufficientFee(s"insufficient fee: $fee"))
    } yield new SetScriptTransaction(chainId, sender, script, fee, timestamp, proofs)
  }

  def signed(sender: PublicKey, script: Option[Script], fee: Long, timestamp: Long, signer: PrivateKey): Either[ValidationError, TransactionT] = {
    create(sender, script, fee, timestamp, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
    }
  }

  def selfSigned(sender: KeyPair, script: Option[Script], fee: Long, timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(sender, script, fee, timestamp, sender)
  }

  val byteTailDescription: ByteEntity[SetScriptTransaction] = {
    (
      OneByte(tailIndex(1), "Chain ID"),
      PublicKeyBytes(tailIndex(2), "Sender's public key"),
      OptionBytes(index = tailIndex(3), name = "Script", nestedByteEntity = ScriptBytes(tailIndex(3), "Script")),
      LongBytes(tailIndex(4), "Fee"),
      LongBytes(tailIndex(5), "Timestamp"),
      ProofsBytes(tailIndex(6))
    ) mapN SetScriptTransaction.apply
  }
}
