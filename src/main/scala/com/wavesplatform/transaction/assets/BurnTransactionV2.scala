package com.wavesplatform.transaction.assets

import com.google.common.primitives.Bytes
import com.wavesplatform.crypto
import monix.eval.Coeval
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.transaction._
import cats.implicits._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.Asset.IssuedAsset

import scala.util.{Failure, Success, Try}

final case class BurnTransactionV2 private (chainId: Byte,
                                            sender: PublicKeyAccount,
                                            asset: IssuedAsset,
                                            quantity: Long,
                                            fee: Long,
                                            timestamp: Long,
                                            proofs: Proofs)
    extends BurnTransaction
    with FastHashId {

  override def builder: TransactionParser = BurnTransactionV2
  override def chainByte: Option[Byte]    = Some(chainId)

  override val bodyBytes: Coeval[Array[Byte]] =
    byteBase.map(base => Bytes.concat(Array(builder.typeId, version, chainId), base))

  override val bytes: Coeval[Array[Byte]] =
    (bodyBytes, proofs.bytes)
      .mapN { case (bb, pb) => Bytes.concat(Array(0: Byte), bb, pb) }

  override def version: Byte = 2
}

object BurnTransactionV2 extends TransactionParserFor[BurnTransactionV2] with TransactionParser.MultipleVersions {

  override val typeId: Byte                 = BurnTransaction.typeId
  override val supportedVersions: Set[Byte] = Set(2)

  override protected def parseTail(bytes: Array[Byte]): Try[BurnTransactionV2] = {
    Try {
      val chainId                                        = bytes(0)
      val (sender, asset, quantity, fee, timestamp, end) = BurnTransaction.parseBase(1, bytes)

      (for {
        proofs <- Proofs.fromBytes(bytes.drop(end))
        tx     <- create(chainId, sender, asset, quantity, fee, timestamp, proofs)
      } yield tx).fold(
        err => Failure(new Exception(err.toString)),
        t => Success(t)
      )
    }.flatten
  }

  def create(chainId: Byte,
             sender: PublicKeyAccount,
             asset: IssuedAsset,
             quantity: Long,
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, BurnTransactionV2] = {
    BurnTransaction
      .validateBurnParams(quantity, fee)
      .map(_ => BurnTransactionV2(chainId, sender, asset, quantity, fee, timestamp, proofs))
  }

  def signed(chainId: Byte,
             sender: PublicKeyAccount,
             asset: IssuedAsset,
             quantity: Long,
             fee: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    for {
      unsigned <- create(chainId, sender, asset, quantity, fee, timestamp, Proofs.empty)
      proofs   <- Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes()))))
    } yield unsigned.copy(proofs = proofs)
  }

  def selfSigned(chainId: Byte,
                 sender: PrivateKeyAccount,
                 asset: IssuedAsset,
                 quantity: Long,
                 fee: Long,
                 timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(chainId, sender, asset, quantity, fee, timestamp, sender)
  }
}
