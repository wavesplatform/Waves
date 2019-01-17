package com.wavesplatform.transaction.lease

import com.google.common.primitives.Bytes
import com.wavesplatform.crypto
import monix.eval.Coeval
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction._
import com.wavesplatform.crypto._

import scala.util.{Failure, Success, Try}

case class LeaseCancelTransactionV1 private (sender: PublicKeyAccount, leaseId: ByteStr, fee: Long, timestamp: Long, signature: ByteStr)
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

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val (sender, fee, timestamp, leaseId, end) = LeaseCancelTransaction.parseBase(bytes, 0)
      val signature                              = ByteStr(bytes.slice(end, KeyLength + 16 + crypto.DigestSize + SignatureLength))
      LeaseCancelTransactionV1
        .create(sender, leaseId, fee, timestamp, signature)
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(sender: PublicKeyAccount, leaseId: ByteStr, fee: Long, timestamp: Long, signature: ByteStr): Either[ValidationError, TransactionT] =
    LeaseCancelTransaction.validateLeaseCancelParams(leaseId, fee).map(_ => LeaseCancelTransactionV1(sender, leaseId, fee, timestamp, signature))

  def signed(sender: PublicKeyAccount,
             leaseId: ByteStr,
             fee: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    create(sender, leaseId, fee, timestamp, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(crypto.sign(signer, unsigned.bodyBytes())))
    }
  }

  def selfSigned(sender: PrivateKeyAccount, leaseId: ByteStr, fee: Long, timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(sender, leaseId, fee, timestamp, sender)
  }
}
