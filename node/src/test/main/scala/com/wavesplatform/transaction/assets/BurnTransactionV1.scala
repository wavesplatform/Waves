package com.wavesplatform.transaction.assets

import cats.implicits._
import com.google.common.primitives.Bytes
import com.wavesplatform.account.{KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.description._
import monix.eval.Coeval

import scala.util.Try

case class BurnTransactionV1 private (sender: PublicKey, asset: IssuedAsset, quantity: Long, fee: Long, timestamp: Long, signature: ByteStr)
    extends BurnTransaction
    with SignedTransaction
    with FastHashId {

  override def version: Byte           = 1
  override def chainByte: Option[Byte] = None

  override val builder: BurnTransactionV1.type = BurnTransactionV1
  override val bodyBytes: Coeval[Array[Byte]]  = byteBase.map(base => Bytes.concat(Array(builder.typeId), base))
  override val bytes: Coeval[Array[Byte]]      = bodyBytes.map(body => Bytes.concat(body, signature.arr))
}

object BurnTransactionV1 extends TransactionParserFor[BurnTransactionV1] with TransactionParser.HardcodedVersion1 {

  override val typeId: Byte = BurnTransaction.typeId

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    byteTailDescription.deserializeFromByteArray(bytes).flatMap { tx =>
      BurnTransaction
        .validateBurnParams(tx)
        .map(_ => tx)
        .foldToTry
    }
  }

  def create(sender: PublicKey,
             asset: IssuedAsset,
             quantity: Long,
             fee: Long,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, TransactionT] = {
    BurnTransaction
      .validateBurnParams(quantity, fee)
      .map(_ => BurnTransactionV1(sender, asset, quantity, fee, timestamp, signature))
  }

  def signed(sender: PublicKey,
             asset: IssuedAsset,
             quantity: Long,
             fee: Long,
             timestamp: Long,
             signer: PrivateKey): Either[ValidationError, TransactionT] = {
    create(sender, asset, quantity, fee, timestamp, ByteStr.empty).right.map { unverified =>
      unverified.copy(signature = ByteStr(crypto.sign(signer, unverified.bodyBytes())))
    }
  }

  def selfSigned(sender: KeyPair, asset: IssuedAsset, quantity: Long, fee: Long, timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(sender, asset, quantity, fee, timestamp, sender)
  }

  val byteTailDescription: ByteEntity[BurnTransactionV1] = {
    (
      PublicKeyBytes(tailIndex(1), "Sender's public key"),
      ByteStrDefinedLength(tailIndex(2), "Asset ID", AssetIdLength).map(IssuedAsset),
      LongBytes(tailIndex(3), "Quantity"),
      LongBytes(tailIndex(4), "Fee"),
      LongBytes(tailIndex(5), "Timestamp"),
      SignatureBytes(tailIndex(6), "Signature")
    ) mapN BurnTransactionV1.apply
  }
}
