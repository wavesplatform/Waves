package com.wavesplatform.transaction.lease

import com.google.common.primitives.Bytes
import com.wavesplatform.crypto
import com.wavesplatform.state.ByteStr
import monix.eval.Coeval
import com.wavesplatform.account.{AddressOrAlias, PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.transaction._
import scorex.crypto.signatures.Curve25519.SignatureLength
import scala.util.{Failure, Success, Try}

case class LeaseTransactionV1 private (sender: PublicKeyAccount,
                                       amount: Long,
                                       fee: Long,
                                       timestamp: Long,
                                       recipient: AddressOrAlias,
                                       signature: ByteStr)
    extends LeaseTransaction
    with SignedTransaction
    with FastHashId {

  override val builder: TransactionParser = LeaseTransactionV1
  val bodyBytes: Coeval[Array[Byte]]      = Coeval.evalOnce(Bytes.concat(Array(builder.typeId), bytesBase()))
  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(bodyBytes(), signature.arr))
  override def version: Byte              = 1
}

object LeaseTransactionV1 extends TransactionParserFor[LeaseTransactionV1] with TransactionParser.HardcodedVersion1 {

  override val typeId: Byte = 8

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      (for {
        parsed <- LeaseTransaction.parseBase(bytes, 0)
        (sender, recipient, quantity, fee, timestamp, end) = parsed
        signature                                          = ByteStr(bytes.slice(end, end + SignatureLength))
        lt <- LeaseTransactionV1.create(sender, quantity, fee, timestamp, recipient, signature)
      } yield lt).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(sender: PublicKeyAccount,
             amount: Long,
             fee: Long,
             timestamp: Long,
             recipient: AddressOrAlias,
             signature: ByteStr): Either[ValidationError, TransactionT] =
    for {
      _ <- LeaseTransaction.validateLeaseParams(amount, fee, recipient, sender)
      _ <- com.wavesplatform.transaction.validation.validateSigLength(signature)
    } yield LeaseTransactionV1(sender, amount, fee, timestamp, recipient, signature)

  def signed(sender: PublicKeyAccount,
             amount: Long,
             fee: Long,
             timestamp: Long,
             recipient: AddressOrAlias,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    create(sender, amount, fee, timestamp, recipient, com.wavesplatform.transaction.validation.EmptySig).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(crypto.sign(signer, unsigned.bodyBytes())))
    }
  }

  def selfSigned(sender: PrivateKeyAccount,
                 amount: Long,
                 fee: Long,
                 timestamp: Long,
                 recipient: AddressOrAlias): Either[ValidationError, TransactionT] = {
    signed(sender, amount, fee, timestamp, recipient, sender)
  }
}
