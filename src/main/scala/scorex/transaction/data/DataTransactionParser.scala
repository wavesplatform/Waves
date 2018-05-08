package scorex.transaction.data

import cats.implicits._
import com.google.common.primitives.{Longs, Shorts}
import com.wavesplatform.state.DataEntry
import scorex.account.{AddressOrAlias, PublicKeyAccount}
import scorex.crypto.signatures.Curve25519.KeyLength
import scorex.transaction.{Proofs, TransactionParser, TransactionParserFor}

import scala.util.{Failure, Success, Try}

object DataTransactionParser extends TransactionParserFor[DataTransaction] with TransactionParser.MultipleVersions {
  override val supportedVersions: Set[Byte] = Set(1, 2)

  override val typeId: Byte = 12

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[DataTransaction] = {
    version match {
      case 1           => parseV1(bytes)
      case 2           => parseV2(bytes)
      case unsupported => Failure(new Exception(s"Unsupported version: $unsupported"))
    }
  }

  private def parseCommon(start: Int, bytes: Array[Byte]) = Try {
    val entryCount = Shorts.fromByteArray(bytes.drop(start))
    val (entries, p1) =
      if (entryCount > 0) {
        val parsed = List.iterate(DataEntry.parse(bytes, start + 2), entryCount) { case (_, p) => DataEntry.parse(bytes, p) }
        (parsed.map(_._1), parsed.last._2)
      } else (List.empty, start + 2)

    val timestamp = Longs.fromByteArray(bytes.drop(p1))
    val feeAmount = Longs.fromByteArray(bytes.drop(p1 + 8))

    (entries, timestamp, feeAmount, p1 + 16)
  }

  private def parseV1(bytes: Array[Byte]): Try[DataTransactionV1] = {
    for {
      sender <- Try(PublicKeyAccount(bytes.slice(0, KeyLength)))
      params <- parseCommon(KeyLength, bytes)
      (entries, timestamp, fee, end) = params
      proofs <- Proofs
        .fromBytes(bytes.drop(end))
        .fold(err => Failure(new Exception(err.toString)), Success.apply)
      tx <- DataTransactionV1
        .create(1, sender, entries, fee, timestamp, proofs)
        .fold(err => Failure(new Exception(err.toString)), Success.apply)
    } yield tx
  }

  private def parseV2(bytes: Array[Byte]): Try[DataTransactionV2] = {
    for {
      sender       <- Try(PublicKeyAccount(bytes.slice(0, KeyLength)))
      hasRecipient <- Try(bytes.slice(KeyLength, KeyLength + 1).head == 1)
      (recipient, offset) <- if (hasRecipient) {
        AddressOrAlias
          .fromBytes(bytes, KeyLength + 1)
          .fold(err => Failure(new Exception(err.toString)), { case (aa, off) => Success((Some(aa), off)) })
      } else Success((none[AddressOrAlias], KeyLength + 1))
      params <- parseCommon(offset, bytes)
      (entries, timestamp, fee, end) = params
      proofs <- Proofs
        .fromBytes(bytes.drop(end))
        .fold(err => Failure(new Exception(err.toString)), Success.apply)
      tx <- DataTransactionV2
        .create(2, sender, recipient, entries, fee, timestamp, proofs)
        .fold(err => Failure(new Exception(err.toString)), Success.apply)
    } yield tx
  }
}
