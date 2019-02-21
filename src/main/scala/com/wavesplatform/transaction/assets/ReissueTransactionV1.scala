package com.wavesplatform.transaction.assets

import com.google.common.primitives.Bytes
import com.wavesplatform.crypto
import monix.eval.Coeval
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction._
import com.wavesplatform.crypto._
import com.wavesplatform.transaction.AssetId.Asset

import scala.util.{Failure, Success, Try}

case class ReissueTransactionV1 private (sender: PublicKeyAccount,
                                         asset: Asset,
                                         quantity: Long,
                                         reissuable: Boolean,
                                         fee: Long,
                                         timestamp: Long,
                                         signature: ByteStr)
    extends ReissueTransaction
    with SignedTransaction
    with FastHashId {

  override val builder: ReissueTransactionV1.type = ReissueTransactionV1
  val bodyBytes: Coeval[Array[Byte]]              = Coeval.evalOnce(Bytes.concat(Array(builder.typeId), bytesBase()))
  override val bytes: Coeval[Array[Byte]]         = Coeval.evalOnce(Bytes.concat(Array(builder.typeId), signature.arr, bodyBytes()))

  override def version: Byte           = 1
  override def chainByte: Option[Byte] = None
}

object ReissueTransactionV1 extends TransactionParserFor[ReissueTransactionV1] with TransactionParser.HardcodedVersion1 {

  override val typeId: Byte = ReissueTransaction.typeId

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    Try {
      val signature = ByteStr(bytes.slice(0, SignatureLength))
      val txId      = bytes(SignatureLength)
      require(txId == typeId, s"Signed tx id is not match")
      val (sender, asset, quantity, reissuable, fee, timestamp, _) = ReissueTransaction.parseBase(bytes, SignatureLength + 1)
      ReissueTransactionV1
        .create(sender, asset, quantity, reissuable, fee, timestamp, signature)
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
  }

  def create(sender: PublicKeyAccount,
             asset: Asset,
             quantity: Long,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, TransactionT] = {
    ReissueTransaction
      .validateReissueParams(quantity, fee)
      .map(_ => ReissueTransactionV1(sender, asset, quantity, reissuable, fee, timestamp, signature))
  }

  def signed(sender: PublicKeyAccount,
             asset: Asset,
             quantity: Long,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    create(sender, asset, quantity, reissuable, fee, timestamp, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(crypto.sign(signer, unsigned.bodyBytes())))
    }
  }

  def selfSigned(sender: PrivateKeyAccount,
                 asset: Asset,
                 quantity: Long,
                 reissuable: Boolean,
                 fee: Long,
                 timestamp: Long): Either[ValidationError, TransactionT] = {
    create(sender, asset, quantity, reissuable, fee, timestamp, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(crypto.sign(sender, unsigned.bodyBytes())))
    }
  }
}
