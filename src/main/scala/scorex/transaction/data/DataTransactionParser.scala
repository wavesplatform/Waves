package scorex.transaction.data

import com.google.common.primitives.{Longs, Shorts}
import com.wavesplatform.state.DataEntry
import scorex.account.{AddressOrAlias, PublicKeyAccount}
import scorex.crypto.signatures.Curve25519.KeyLength
import scorex.transaction.{Proofs, TransactionParser, TransactionParserFor, ValidationError}
import cats.implicits._

import scala.util.{Failure, Success, Try}
import com.wavesplatform.state._
import scorex.serialization.Deser

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

  private def parseCommon(bytes: Array[Byte]) = Try {
    val entryCount = Shorts.fromByteArray(bytes)
    val (entries, p1) =
      if (entryCount > 0) {
        val parsed = List.iterate(DataEntry.parse(bytes, 2), entryCount) { case (_, p) => DataEntry.parse(bytes, p) }
        (parsed.map(_._1), parsed.last._2)
      } else (List.empty, 2)

    val timestamp = Longs.fromByteArray(bytes.drop(p1))
    val feeAmount = Longs.fromByteArray(bytes.drop(p1 + 8))

    (entries, timestamp, feeAmount, p1 + 8)
  }

  private def parseV1(bytes: Array[Byte]): Try[DataTransactionV1] = {
    for {
      sender <- Try(PublicKeyAccount(bytes.slice(0, KeyLength)))
      params <- parseCommon(bytes.drop(KeyLength))
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
      sender             <- Try(PublicKeyAccount(bytes.slice(0, KeyLength)))
      (recBytes, offset) <- Try(Deser.parseByteArrayOption(bytes, KeyLength, 24))
      recipient <- recBytes
        .traverse[({ type L[A] = Either[ValidationError, A] })#L, AddressOrAlias](rb => AddressOrAlias.fromBytes(rb, 0).map(_._1))
        .fold(
          err => Failure(new Exception(err.toString)),
          opt => Success(opt)
        )
      params <- parseCommon(bytes.drop(offset))
      (entries, timestamp, fee, end) = params
      proofs <- Proofs
        .fromBytes(bytes.drop(end))
        .fold(err => Failure(new Exception(err.toString)), Success.apply)
      tx <- DataTransactionV2
        .create(1, sender, recipient, entries, fee, timestamp, proofs)
        .fold(err => Failure(new Exception(err.toString)), Success.apply)
    } yield tx
  }
}
