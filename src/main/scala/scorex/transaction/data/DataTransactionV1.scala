package scorex.transaction.data

import cats.implicits._
import com.google.common.primitives.Bytes
import com.wavesplatform.crypto
import com.wavesplatform.state._
import monix.eval.Coeval
import play.api.libs.json._
import scorex.account.{AddressOrAlias, PrivateKeyAccount, PublicKeyAccount}
import scorex.transaction._

case class DataTransactionV1 private (version: Byte, sender: PublicKeyAccount, data: List[DataEntry[_]], fee: Long, timestamp: Long, proofs: Proofs)
    extends DataTransaction
    with FastHashId {

  override def recipient: Option[AddressOrAlias] = None

  override val builder: TransactionParser = DataTransactionParser

  override val bodyBytes: Coeval[Array[Byte]] = baseBytes.map(bb => version +: builder.typeId +: bb)

  implicit val dataItemFormat: Format[DataEntry[_]] = DataEntry.Format

  override val bytes: Coeval[Array[Byte]] = (bodyBytes, proofs.bytes)
    .mapN {
      case (bb, pb) =>
        Bytes.concat(Array(0: Byte), bb, pb)
    }
}

object DataTransactionV1 {

  def create(version: Byte,
             sender: PublicKeyAccount,
             data: List[DataEntry[_]],
             feeAmount: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, DataTransactionV1] = {
    DataTransaction
      .validate(Set(1), version, data, feeAmount)
      .map(_ => DataTransactionV1(version, sender, data, feeAmount, timestamp, proofs))
  }

  def selfSigned(version: Byte,
                 sender: PrivateKeyAccount,
                 data: List[DataEntry[_]],
                 feeAmount: Long,
                 timestamp: Long): Either[ValidationError, DataTransactionV1] = {
    for {
      unsigned <- create(version, sender, data, feeAmount, timestamp, Proofs.empty)
      proofs   <- Proofs.create(Seq(ByteStr(crypto.sign(sender, unsigned.bodyBytes()))))
    } yield unsigned.copy(proofs = proofs)
  }
}
