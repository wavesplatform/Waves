package com.wavesplatform.transaction.assets

import com.google.common.primitives.Bytes
import com.wavesplatform.crypto
import com.wavesplatform.state.ByteStr
import monix.eval.Coeval
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.smart.script.Script
import scorex.crypto.signatures.Curve25519.SignatureLength
import scala.util.{Failure, Success, Try}

case class IssueTransactionV1 private (sender: PublicKeyAccount,
                                       name: Array[Byte],
                                       description: Array[Byte],
                                       quantity: Long,
                                       decimals: Byte,
                                       reissuable: Boolean,
                                       fee: Long,
                                       timestamp: Long,
                                       signature: ByteStr)
    extends IssueTransaction
    with SignedTransaction
    with FastHashId {
  override val version: Byte                    = 1
  override val script: Option[Script]           = None
  override val builder: IssueTransactionV1.type = IssueTransactionV1
  override val bodyBytes: Coeval[Array[Byte]]   = Coeval.evalOnce(Bytes.concat(Array(builder.typeId), bytesBase()))
  override val bytes: Coeval[Array[Byte]]       = Coeval.evalOnce(Bytes.concat(Array(builder.typeId), signature.arr, bodyBytes()))

}

object IssueTransactionV1 extends TransactionParserFor[IssueTransactionV1] with TransactionParser.HardcodedVersion1 {

  override val typeId: Byte = 3

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val signature = ByteStr(bytes.slice(0, SignatureLength))
      val txId      = bytes(SignatureLength)
      require(txId == typeId, s"Signed tx id is not match")
      val (sender, assetName, description, quantity, decimals, reissuable, fee, timestamp, _) = IssueTransaction.parseBase(bytes, SignatureLength + 1)
      IssueTransactionV1
        .create(sender, assetName, description, quantity, decimals, reissuable, fee, timestamp, signature)
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(sender: PublicKeyAccount,
             name: Array[Byte],
             description: Array[Byte],
             quantity: Long,
             decimals: Byte,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, TransactionT] =
    for {
      _ <- IssueTransaction.validateIssueParams(name, description, quantity, decimals, reissuable, fee)
      _ <- com.wavesplatform.transaction.validation.validateSigLength(signature)
    } yield IssueTransactionV1(sender, name, description, quantity, decimals, reissuable, fee, timestamp, signature)

  def signed(sender: PublicKeyAccount,
             name: Array[Byte],
             description: Array[Byte],
             quantity: Long,
             decimals: Byte,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] =
    create(sender, name, description, quantity, decimals, reissuable, fee, timestamp, com.wavesplatform.transaction.validation.EmptySig).right.map {
      unverified =>
        unverified.copy(signature = ByteStr(crypto.sign(signer, unverified.bodyBytes())))
    }

  def selfSigned(sender: PrivateKeyAccount,
                 name: Array[Byte],
                 description: Array[Byte],
                 quantity: Long,
                 decimals: Byte,
                 reissuable: Boolean,
                 fee: Long,
                 timestamp: Long): Either[ValidationError, TransactionT] =
    signed(sender, name, description, quantity, decimals, reissuable, fee, timestamp, sender)

}
