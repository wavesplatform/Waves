package com.wavesplatform.transaction.lease

import cats.implicits._
import com.google.common.primitives.Bytes
import com.wavesplatform.account.{AddressScheme, KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.description._
import monix.eval.Coeval

import scala.util.Try

case class LeaseCancelTransactionV2 private (chainId: Byte, sender: PublicKey, leaseId: ByteStr, fee: Long, timestamp: Long, proofs: Proofs)
    extends LeaseCancelTransaction
    with FastHashId {

  override def chainByte: Option[Byte] = Some(chainId)

  override val builder: TransactionParser = LeaseCancelTransactionV2

  val bodyBytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(Bytes.concat(Array(builder.typeId, version, chainId), bytesBase()))

  override val bytes = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))

  override def version: Byte = 2
}

object LeaseCancelTransactionV2 extends TransactionParserFor[LeaseCancelTransactionV2] with TransactionParser.MultipleVersions {

  override val typeId: Byte = LeaseCancelTransaction.typeId

  override def supportedVersions: Set[Byte] = Set(2)
  private def currentChainId: Byte          = AddressScheme.current.chainId

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    byteTailDescription.deserializeFromByteArray(bytes).flatMap { tx =>
      Either
        .cond(tx.chainId == currentChainId, (), GenericError(s"Wrong chainId actual: ${tx.chainId.toInt}, expected: $currentChainId"))
        .flatMap(_ => LeaseCancelTransaction.validateLeaseCancelParams(tx))
        .map(_ => tx)
        .foldToTry
    }
  }

  def create(chainId: Byte,
             sender: PublicKey,
             leaseId: ByteStr,
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, TransactionT] = {
    for {
      _ <- Either.cond(chainId == currentChainId, (), GenericError(s"Wrong chainId actual: ${chainId.toInt}, expected: $currentChainId"))
      _ <- LeaseCancelTransaction.validateLeaseCancelParams(leaseId, fee)
    } yield LeaseCancelTransactionV2(chainId, sender, leaseId, fee, timestamp, proofs)
  }

  def signed(chainId: Byte,
             sender: PublicKey,
             leaseId: ByteStr,
             fee: Long,
             timestamp: Long,
             signer: PrivateKey): Either[ValidationError, TransactionT] = {
    create(chainId, sender, leaseId, fee, timestamp, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
    }
  }

  def selfSigned(chainId: Byte, sender: KeyPair, leaseId: ByteStr, fee: Long, timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(chainId, sender, leaseId, fee, timestamp, sender)
  }

  val byteTailDescription: ByteEntity[LeaseCancelTransactionV2] = {
    (
      OneByte(tailIndex(1), "Chain ID"),
      PublicKeyBytes(tailIndex(2), "Sender's public key"),
      LongBytes(tailIndex(3), "Fee"),
      LongBytes(tailIndex(4), "Timestamp"),
      ByteStrDefinedLength(tailIndex(5), "Lease ID", crypto.DigestSize),
      ProofsBytes(tailIndex(6))
    ) mapN {
      case (chainId, senderPublicKey, fee, timestamp, leaseId, proofs) =>
        LeaseCancelTransactionV2(
          chainId = chainId,
          sender = senderPublicKey,
          leaseId = leaseId,
          fee = fee,
          timestamp = timestamp,
          proofs = proofs
        )
    }
  }
}
