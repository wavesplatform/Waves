package scorex.transaction.data

import com.wavesplatform.state.{ByteStr, DataEntry}
import monix.eval.Coeval
import scorex.account.{AddressOrAlias, PrivateKeyAccount, PublicKeyAccount}
import scorex.transaction.{FastHashId, Proofs, TransactionParser, ValidationError}
import cats.implicits._
import com.google.common.primitives.Bytes
import com.wavesplatform.crypto

final case class DataTransactionV2 private (version: Byte,
                                            sender: PublicKeyAccount,
                                            recipient: Option[AddressOrAlias],
                                            data: List[DataEntry[_]],
                                            fee: Long,
                                            timestamp: Long,
                                            proofs: Proofs)
    extends DataTransaction
    with FastHashId {

  override def builder: TransactionParser = DataTransactionParser

  override val bodyBytes: Coeval[Array[Byte]] =
    baseBytes.map(bb => version +: builder.typeId +: bb)

  override val bytes: Coeval[Array[Byte]] =
    (bodyBytes, proofs.bytes)
      .mapN { case (bb, pb) => Bytes.concat(Array(0: Byte), bb, pb) }
}

object DataTransactionV2 {
  def create(version: Byte,
             sender: PublicKeyAccount,
             recipient: Option[AddressOrAlias],
             data: List[DataEntry[_]],
             feeAmount: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, DataTransactionV2] = {
    DataTransaction
      .validate(Set(2), version, data, feeAmount)
      .map(_ => DataTransactionV2(version, sender, recipient, data, feeAmount, timestamp, proofs))
  }

  def selfSigned(version: Byte,
                 sender: PrivateKeyAccount,
                 recipient: Option[AddressOrAlias],
                 data: List[DataEntry[_]],
                 feeAmount: Long,
                 timestamp: Long): Either[ValidationError, DataTransactionV2] = {
    for {
      unsigned <- create(version, sender, recipient, data, feeAmount, timestamp, Proofs.empty)
      proofs   <- Proofs.create(Seq(ByteStr(crypto.sign(sender, unsigned.bodyBytes()))))
    } yield unsigned.copy(proofs = proofs)
  }
}
