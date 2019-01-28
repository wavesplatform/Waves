package com.wavesplatform.transaction.assets

import com.google.common.primitives.Bytes
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.crypto.SignatureLength
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.smart.script.Script
import monix.eval.Coeval
import play.api.libs.json.JsObject

import com.wavesplatform.transaction.description._

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
  override val json: Coeval[JsObject]           = issueJson

}

object IssueTransactionV1 extends TransactionParserFor[IssueTransactionV1] with TransactionParser.HardcodedVersion1 {

  override val typeId: Byte = IssueTransaction.typeId

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
    IssueTransaction
      .validateIssueParams(name, description, quantity, decimals, reissuable, fee)
      .map(_ => IssueTransactionV1(sender, name, description, quantity, decimals, reissuable, fee, timestamp, signature))

  def signed(sender: PublicKeyAccount,
             name: Array[Byte],
             description: Array[Byte],
             quantity: Long,
             decimals: Byte,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] =
    create(sender, name, description, quantity, decimals, reissuable, fee, timestamp, ByteStr.empty).right.map { unverified =>
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

  /*override */
  val byteDesc =
    (OneByte("Transaction type (3)") ~
      com.wavesplatform.transaction.description.BytesArrayDefinedLength("Signature", SignatureLength) ~
      OneByte("Transaction type 2 (3)") ~
      PublicKeyAccountBytes("Sender's public key") ~
      BytesArrayUndefinedLength("Asset name") ~
      BytesArrayUndefinedLength("Description") ~
      LongBytes("Quantity") ~
      OneByte("Decimals") ~
      BooleanByte("Reissuable") ~
      LongBytes("Fee") ~
      LongBytes("Timestamp"))
      .map {
        case ((((((((((txType1, signature), txType2), senderPublicKey), name), desc), quantity), decimals), reissuable), fee), timestamp) =>
          IssueTransactionV1
            .create(
              sender = senderPublicKey,
              name = name,
              description = desc,
              quantity = quantity,
              decimals = decimals,
              reissuable = reissuable,
              fee = fee,
              timestamp = timestamp,
              signature = ByteStr(signature)
            )
            .right
            .get
      }

  def getDocs() = byteDesc.generateDoc()

}

object Test extends App {

  import com.wavesplatform.common.utils.EitherExt2

  val doc = IssueTransactionV1.byteDesc.generateDoc()

  val itv1 = IssueTransactionV1
    .create(
      PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
      "Gigacoin".getBytes,
      "Gigacoin".getBytes,
      10000000000L,
      8,
      true,
      100000000,
      1526287561757L,
      ByteStr.decodeBase58("28kE1uN1pX2bwhzr9UHw5UuB9meTFEDFgeunNgy6nZWpHX4pzkGYotu8DhQ88AdqUG6Yy5wcXgHseKPBUygSgRMJ").get
    )
    .right
    .get

  val itv1d = IssueTransactionV1.byteDesc.deserializeFromByteArray(itv1.bytes.value)

  println(doc)
  println(itv1d)

}
