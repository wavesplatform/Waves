package com.wavesplatform.transaction.lease

import cats.implicits._
import com.google.common.primitives.Bytes
import com.wavesplatform.account.{KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.description._
import monix.eval.Coeval

import scala.util.Try

case class LeaseCancelTransactionV1 private (sender: PublicKey, leaseId: ByteStr, fee: Long, timestamp: Long, signature: ByteStr)
    extends LeaseCancelTransaction
    with SignedTransaction
    with FastHashId {

  override val builder: TransactionParser = LeaseCancelTransactionV1

  override def chainByte: Option[Byte] = None

  val bodyBytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(Bytes.concat(Array(builder.typeId), bytesBase()))

  override val bytes = Coeval.evalOnce(Bytes.concat(bodyBytes(), signature.arr))

  override def version: Byte = 1
}

object LeaseCancelTransactionV1 extends TransactionParserFor[LeaseCancelTransactionV1] with TransactionParser.HardcodedVersion1 {

  override val typeId: Byte = LeaseCancelTransaction.typeId

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    byteTailDescription.deserializeFromByteArray(bytes).flatMap { tx =>
      LeaseCancelTransaction
        .validateLeaseCancelParams(tx)
        .map(_ => tx)
        .foldToTry
    }
  }

  def create(sender: PublicKey, leaseId: ByteStr, fee: Long, timestamp: Long, signature: ByteStr): Either[ValidationError, TransactionT] = {
    LeaseCancelTransaction.validateLeaseCancelParams(leaseId, fee).map(_ => LeaseCancelTransactionV1(sender, leaseId, fee, timestamp, signature))
  }

  def signed(sender: PublicKey, leaseId: ByteStr, fee: Long, timestamp: Long, signer: PrivateKey): Either[ValidationError, TransactionT] = {
    create(sender, leaseId, fee, timestamp, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(crypto.sign(signer, unsigned.bodyBytes())))
    }
  }

  def selfSigned(sender: KeyPair, leaseId: ByteStr, fee: Long, timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(sender, leaseId, fee, timestamp, sender)
  }

  val byteTailDescription: ByteEntity[LeaseCancelTransactionV1] = {
    (
      PublicKeyBytes(tailIndex(1), "Sender's public key"),
      LongBytes(tailIndex(2), "Fee"),
      LongBytes(tailIndex(3), "Timestamp"),
      ByteStrDefinedLength(tailIndex(4), "Lease ID", crypto.DigestSize),
      SignatureBytes(tailIndex(5), "Signature")
    ) mapN {
      case (sender, fee, timestamp, leaseId, signature) =>
        LeaseCancelTransactionV1(
          sender = sender,
          leaseId = leaseId,
          fee = fee,
          timestamp = timestamp,
          signature = signature
        )
    }
  }
}
