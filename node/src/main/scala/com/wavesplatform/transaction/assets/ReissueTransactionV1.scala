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

case class ReissueTransactionV1 private (sender: PublicKey,
                                         asset: IssuedAsset,
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
    byteTailDescription.deserializeFromByteArray(bytes).flatMap { tx =>
      ReissueTransaction
        .validateReissueParams(tx)
        .map(_ => tx)
        .foldToTry
    }
  }

  def create(sender: PublicKey,
             asset: IssuedAsset,
             quantity: Long,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, TransactionT] = {
    ReissueTransaction
      .validateReissueParams(quantity, fee)
      .map(_ => ReissueTransactionV1(sender, asset, quantity, reissuable, fee, timestamp, signature))
  }

  def signed(sender: PublicKey,
             asset: IssuedAsset,
             quantity: Long,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long,
             signer: PrivateKey): Either[ValidationError, TransactionT] = {
    create(sender, asset, quantity, reissuable, fee, timestamp, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(crypto.sign(signer, unsigned.bodyBytes())))
    }
  }

  def selfSigned(sender: KeyPair,
                 asset: IssuedAsset,
                 quantity: Long,
                 reissuable: Boolean,
                 fee: Long,
                 timestamp: Long): Either[ValidationError, TransactionT] = {
    create(sender, asset, quantity, reissuable, fee, timestamp, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(crypto.sign(sender, unsigned.bodyBytes())))
    }
  }

  val byteTailDescription: ByteEntity[ReissueTransactionV1] = {
    (
      SignatureBytes(tailIndex(1), "Signature"),
      ConstantByte(tailIndex(2), value = typeId, name = "Transaction type"),
      PublicKeyBytes(tailIndex(3), "Sender's public key"),
      ByteStrDefinedLength(tailIndex(4), "Asset ID", AssetIdLength),
      LongBytes(tailIndex(5), "Quantity"),
      BooleanByte(tailIndex(6), "Reissuable flag (1 - True, 0 - False)"),
      LongBytes(tailIndex(7), "Fee"),
      LongBytes(tailIndex(8), "Timestamp")
    ) mapN {
      case (signature, txId, sender, assetId, quantity, reissuable, fee, timestamp) =>
        require(txId == typeId, s"Signed tx id is not match")
        ReissueTransactionV1(
          sender = sender,
          asset = IssuedAsset(assetId),
          quantity = quantity,
          reissuable = reissuable,
          fee = fee,
          timestamp = timestamp,
          signature = signature
        )
    }
  }
}
