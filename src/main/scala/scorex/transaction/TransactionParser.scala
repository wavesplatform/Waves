package scorex.transaction

import com.google.common.primitives.Longs
import com.wavesplatform.state.ByteStr
import scorex.account.{Alias, PublicKeyAccount}
import scorex.crypto.signatures.Curve25519.KeyLength
import scorex.transaction.modern.{TxData, TxHeader}
import scorex.transaction.validation._

import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

trait TransactionParser {
  type TransactionT <: Transaction
  def classTag: ClassTag[TransactionT]

  def typeId: Byte
  def supportedVersions: Set[Byte]

  def parseBytes(bytes: Array[Byte]): Try[TransactionT] = parseHeader(bytes).flatMap {
    case (version, offset) =>
      parseTail(version, bytes.drop(offset))
  }

  /**
    * @return (version, offset)
    */
  protected def parseHeader(bytes: Array[Byte]): Try[(Byte, Int)]
  protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT]
}

object TransactionParser {
  type Aux[T <: Transaction] = TransactionParser { type TransactionT = T }
  trait HardcodedVersion1 extends TransactionParser {
    override val supportedVersions: Set[Byte] = Set(1)

    override protected def parseHeader(bytes: Array[Byte]): Try[(Byte, Int)] = Try {
      if (bytes.length < 1) throw new IllegalArgumentException(s"The buffer is too small, it has ${bytes.length} elements")

      val parsedTypeId = bytes.head
      if (parsedTypeId != typeId) throw new IllegalArgumentException(s"Expected type of transaction '$typeId', but got '$parsedTypeId'")
      (1, 1)
    }
  }

  trait OneVersion extends TransactionParser {
    def version: Byte
    override def supportedVersions: Set[Byte] = Set(version)

    override protected def parseHeader(bytes: Array[Byte]): Try[(Byte, Int)] = Try {
      if (bytes.length < 2) throw new IllegalArgumentException(s"The buffer is too small, it has ${bytes.length} elements")

      val Array(parsedTypeId, parsedVersion) = bytes.take(2)
      if (parsedTypeId != typeId) throw new IllegalArgumentException(s"Expected type of transaction '$typeId', but got '$parsedTypeId'")
      if (!supportedVersions.contains(parsedVersion))
        throw new IllegalArgumentException(s"Expected version of transaction: $version, but got '$parsedVersion'")

      (parsedVersion, 2)
    }
  }

  trait MultipleVersions extends TransactionParser {
    override protected def parseHeader(bytes: Array[Byte]): Try[(Byte, Int)] = Try {
      if (bytes.length < 3) throw new IllegalArgumentException(s"The buffer is too small, it has ${bytes.length} elements")

      val Array(parsedMark, parsedTypeId, parsedVersion) = bytes.take(3)
      if (parsedMark != 0) throw new IllegalArgumentException(s"Expected the '0' byte, but got '$parsedMark'")
      if (parsedTypeId != typeId) throw new IllegalArgumentException(s"Expected type of transaction '$typeId', but got '$parsedTypeId'")
      if (!supportedVersions.contains(parsedVersion))
        throw new IllegalArgumentException(s"Expected version of transaction ${supportedVersions.mkString(", ")}, but got '$parsedVersion'")

      (parsedVersion, 3)
    }

    def parsePK(bytes: Array[Byte]): Try[PublicKeyAccount] = {
      Try(PublicKeyAccount(bytes))
    }

    def parseSenderAndTimestamp(bytes: Array[Byte]): Try[(PublicKeyAccount, Long)] = {
      val (pkStart, pkEnd) = (0, KeyLength)
      val (tsStart, tsEnd) = (KeyLength, KeyLength + 8)

      for {
        pk <- Try(PublicKeyAccount(bytes.slice(pkStart, pkEnd)))
        ts <- parseLong(bytes.slice(tsStart, tsEnd))
      } yield (pk, ts)
    }

    def parseAlias(bytes: Array[Byte]): Try[Alias] = {
      Alias
        .fromBytes(bytes)
        .fold(
          ve => Failure(new Exception(ve.toString)),
          a => Success(a)
        )
    }

    def parseByteStr(bytes: Array[Byte]): Try[ByteStr] = Try {
      ByteStr(bytes)
    }

    def parseLong(bytes: Array[Byte]): Try[Long] = Try {
      Longs.fromByteArray(bytes)
    }

    def parseProofs(bytes: Array[Byte]): Try[Proofs] = {
      Proofs
        .fromBytes(bytes)
        .fold(
          ve => Failure(new Exception(ve.toString)),
          ps => Success(ps)
        )
    }
  }

  abstract class Modern[T <: Transaction, D <: TxData](implicit override val classTag: ClassTag[T]) extends TransactionParser {

    override type TransactionT = T

    def create(header: TxHeader, data: D, proofs: Proofs): Try[TransactionT]

    override protected def parseHeader(bytes: Array[Byte]): Try[(Byte, Int)] =
      (for {
        _ <- Either.cond(bytes.length < 3, (), new IllegalArgumentException(s"The buffer is too small, it has ${bytes.length} elements"))
        Array(parsedMark, parsedTypeId, parsedVersion) = bytes.take(3)
        _ <- Either.cond(parsedMark != 0, (), new IllegalArgumentException(s"Expected the '0' byte, but got '$parsedMark'"))
        _ <- Either.cond(parsedTypeId != typeId, (), new IllegalArgumentException(s"Expected type of transaction '$typeId', but got '$parsedTypeId'"))
      } yield (parsedVersion, 3)).toTry

    def parseTxData(version: Byte, bytes: Array[Byte]): Try[(D, Int)]

    override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] = {
      val (pkStart, pkEnd)   = (0, KeyLength)
      val (tsStart, tsEnd)   = (KeyLength, KeyLength + 8)
      val (feeStart, feeEnd) = (tsEnd, tsEnd + 8)

      for {
        sender    <- parsePK(bytes.slice(pkStart, pkEnd))
        timestamp <- parseLong(bytes.slice(tsStart, tsEnd))
        fee       <- parseLong(bytes.slice(feeStart, feeEnd))
        header <- ValidateModern
          .header(supportedVersions)(typeId, version, sender, fee, timestamp)
          .fold(
            errs => Failure(new Exception(errs.toString)),
            succ => Success(succ)
          )
        (data, offset) <- parseTxData(version, bytes.drop(feeEnd))
        proofs         <- parseProofs(bytes.drop(feeEnd + offset))
        tx             <- create(header, data, proofs)
      } yield tx
    }

    def parsePK(bytes: Array[Byte]): Try[PublicKeyAccount] = {
      Try(PublicKeyAccount(bytes))
    }

    def parseAlias(bytes: Array[Byte]): Try[Alias] = {
      Alias
        .fromBytes(bytes)
        .fold(
          ve => Failure(new Exception(ve.toString)),
          a => Success(a)
        )
    }

    def parseByteStr(bytes: Array[Byte]): Try[ByteStr] = Try {
      ByteStr(bytes)
    }

    def parseLong(bytes: Array[Byte]): Try[Long] = Try {
      Longs.fromByteArray(bytes)
    }

    def parseProofs(bytes: Array[Byte]): Try[Proofs] = {
      Proofs
        .fromBytes(bytes)
        .fold(
          ve => Failure(new Exception(ve.toString)),
          ps => Success(ps)
        )
    }
  }

}

abstract class TransactionParserFor[T <: Transaction](implicit override val classTag: ClassTag[T]) extends TransactionParser {
  override type TransactionT = T
}
