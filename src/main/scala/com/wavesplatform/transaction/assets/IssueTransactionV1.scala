package com.wavesplatform.transaction.assets

import cats.implicits._
import com.google.common.primitives.Bytes
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.description._
import com.wavesplatform.transaction.smart.script.Script
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.util.Try

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
  override val builder: IssueTransactionV1.type = IssueTransactionV1
  override val bodyBytes: Coeval[Array[Byte]]   = Coeval.evalOnce(Bytes.concat(Array(builder.typeId), bytesBase()))
  override val bytes: Coeval[Array[Byte]]       = Coeval.evalOnce(Bytes.concat(Array(builder.typeId), signature.arr, bodyBytes()))
  override val script: Option[Script]           = None
  override val json: Coeval[JsObject]           = issueJson
}

object IssueTransactionV1 extends TransactionParserFor[IssueTransactionV1] with TransactionParser.HardcodedVersion1 {

  override val typeId: Byte = IssueTransaction.typeId

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    byteTailDescription.deserializeFromByteArray(bytes).flatMap { tx =>
      IssueTransaction
        .validateIssueParams(tx)
        .map(_ => tx)
        .foldToTry
    }
  }

  def create(sender: PublicKeyAccount,
             name: Array[Byte],
             description: Array[Byte],
             quantity: Long,
             decimals: Byte,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, TransactionT] = {
    IssueTransaction
      .validateIssueParams(name, description, quantity, decimals, reissuable, fee)
      .map(_ => IssueTransactionV1(sender, name, description, quantity, decimals, reissuable, fee, timestamp, signature))
  }

  def signed(sender: PublicKeyAccount,
             name: Array[Byte],
             description: Array[Byte],
             quantity: Long,
             decimals: Byte,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    create(sender, name, description, quantity, decimals, reissuable, fee, timestamp, ByteStr.empty).right.map { unverified =>
      unverified.copy(signature = ByteStr(crypto.sign(signer, unverified.bodyBytes())))
    }
  }

  def selfSigned(sender: PrivateKeyAccount,
                 name: Array[Byte],
                 description: Array[Byte],
                 quantity: Long,
                 decimals: Byte,
                 reissuable: Boolean,
                 fee: Long,
                 timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(sender, name, description, quantity, decimals, reissuable, fee, timestamp, sender)
  }

  val byteTailDescription: ByteEntity[IssueTransactionV1] = {
    (
      SignatureBytes(tailIndex(1), "Signature"),
      ConstantByte(tailIndex(2), value = typeId, name = "Transaction type"),
      PublicKeyAccountBytes(tailIndex(3), "Sender's public key"),
      BytesArrayUndefinedLength(tailIndex(4), "Asset name"),
      BytesArrayUndefinedLength(tailIndex(5), "Description"),
      LongBytes(tailIndex(6), "Quantity"),
      OneByte(tailIndex(7), "Decimals"),
      BooleanByte(tailIndex(8), "Reissuable flag (1 - True, 0 - False)"),
      LongBytes(tailIndex(9), "Fee"),
      LongBytes(tailIndex(10), "Timestamp")
    ) mapN {
      case (signature, txId, senderPublicKey, name, desc, quantity, decimals, reissuable, fee, timestamp) =>
        require(txId == typeId, s"Signed tx id is not match")
        IssueTransactionV1(
          sender = senderPublicKey,
          name = name,
          description = desc,
          quantity = quantity,
          decimals = decimals,
          reissuable = reissuable,
          fee = fee,
          timestamp = timestamp,
          signature = signature
        )
    }
  }
}
