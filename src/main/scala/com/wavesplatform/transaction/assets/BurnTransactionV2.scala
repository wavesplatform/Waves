package com.wavesplatform.transaction.assets

import com.google.common.primitives.Bytes
import com.wavesplatform.crypto
import monix.eval.Coeval
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.transaction._
import cats.implicits._
import com.wavesplatform.common.state.ByteStr

import scala.util.{Failure, Success, Try}

final case class BurnTransactionV2 private (version: Byte,
                                            chainId: Byte,
                                            sender: PublicKeyAccount,
                                            assetId: ByteStr,
                                            quantity: Long,
                                            fee: Long,
                                            timestamp: Long,
                                            proofs: Proofs)
    extends BurnTransaction
    with FastHashId {
  override def builder: TransactionParser = BurnTransactionV2

  override def chainByte: Option[Byte] = Some(chainId)

  override val bodyBytes: Coeval[Array[Byte]] =
    byteBase.map(base => Bytes.concat(Array(builder.typeId, version, chainId), base))

  override val bytes: Coeval[Array[Byte]] =
    (bodyBytes, proofs.bytes)
      .mapN { case (bb, pb) => Bytes.concat(Array(0: Byte), bb, pb) }
}

object BurnTransactionV2 extends TransactionParserFor[BurnTransactionV2] with TransactionParser.MultipleVersions {
  override val typeId: Byte = BurnTransaction.typeId

  override val supportedVersions: Set[Byte] = Set(2)

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[BurnTransactionV2] =
    Try {
      val chainId                                          = bytes(0)
      val (sender, assetId, quantity, fee, timestamp, end) = BurnTransaction.parseBase(1, bytes)

      (for {
        proofs <- Proofs.fromBytes(bytes.drop(end))
        tx     <- create(version, chainId, sender, assetId, quantity, fee, timestamp, proofs)
      } yield tx).fold(
        err => Failure(new Exception(err.toString)),
        t => Success(t)
      )
    }.flatten

  def create(version: Byte,
             chainId: Byte,
             sender: PublicKeyAccount,
             assetId: ByteStr,
             quantity: Long,
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, BurnTransactionV2] =
    BurnTransaction
      .validateBurnParams(quantity, fee)
      .map(_ => BurnTransactionV2(version, chainId, sender, assetId, quantity, fee, timestamp, proofs))

  def signed(version: Byte,
             chainId: Byte,
             sender: PublicKeyAccount,
             assetId: ByteStr,
             quantity: Long,
             fee: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] =
    for {
      unsigned <- create(version, chainId, sender, assetId, quantity, fee, timestamp, Proofs.empty)
      proofs   <- Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes()))))
    } yield unsigned.copy(proofs = proofs)

  def selfSigned(version: Byte,
                 chainId: Byte,
                 sender: PrivateKeyAccount,
                 assetId: ByteStr,
                 quantity: Long,
                 fee: Long,
                 timestamp: Long): Either[ValidationError, TransactionT] =
    signed(version, chainId, sender, assetId, quantity, fee, timestamp, sender)
}
