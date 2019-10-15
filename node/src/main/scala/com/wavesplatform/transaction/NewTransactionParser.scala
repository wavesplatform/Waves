package com.wavesplatform.transaction

import scala.util.{Failure, Success, Try}

// todo: (NODE-1915) Rename
trait NewTransactionParser[T <: Transaction] {
  def transactionType: TransactionType
  def supportedVersions: Set[TransactionVersion]
  def parseBytes(bytes: Array[Byte]): Try[T]
}

object NewTransactionParser {

  val HardcodedV1Offset  = 1
  val OneVersionOffset   = 2
  val MultiVersionOffset = 3

  def parseTypeId(bytes: Array[Byte]): Try[TransactionType] =
    bytes match {
      case modernTypeId(typeId) => Success(typeId)
      case oldTypeId(typeId)    => Success(typeId)
      case data if data.isEmpty => Failure(new IllegalArgumentException("Can't find the significant byte: the buffer is empty"))
      case data                 => Failure(new IllegalArgumentException(s"Can't determine the type of transaction: the buffer has ${data.length} bytes"))
    }

  def oldBodyOffset(bytes: Array[Byte]): Try[Int] =
    for {
      _ <- expectedMinSize(bytes, 2)
    } yield OneVersionOffset

  def modernBodyOffset(bytes: Array[Byte]): Try[Int] =
    for {
      _ <- expectedMinSize(bytes, 3)
      _ <- Either.cond(bytes.head == 0, (), new IllegalArgumentException(s"Expected the '0' byte, but got '${bytes.head}'")).toTry
    } yield MultiVersionOffset

  def expectedMinSize(bytes: Array[Byte], size: Int): Try[Unit] =
    Either
      .cond(
        bytes.length >= size,
        (),
        new IllegalArgumentException(s"The buffer is too small, it has ${bytes.length} elements")
      )
      .toTry

  def expectedType(parsedTypeId: TransactionType, expectedTypeId: TransactionType): Try[Unit] =
    Either
      .cond(
        parsedTypeId == expectedTypeId,
        (),
        new IllegalArgumentException(s"Expected type of transaction '$expectedTypeId', but got '$parsedTypeId'")
      )
      .toTry

  def expectedVersion(parsedVersion: TransactionVersion, supportedVersions: Set[TransactionVersion]): Try[Unit] =
    Either
      .cond(
        supportedVersions.contains(parsedVersion),
        (),
        new IllegalArgumentException(s"Expected version of transaction ${supportedVersions.mkString(", ")}, but got '$parsedVersion'")
      )
      .toTry

  private[this] object modernTypeId {
    def unapply(bytes: Array[Byte]): Option[TransactionType] =
      bytes match {
        case data if data.length >= 2 && data.head == 0 => Some(TransactionType(data(1)))
        case _                                          => None
      }
  }

  private[this] object oldTypeId {
    def unapply(bytes: Array[Byte]): Option[TransactionType] =
      bytes match {
        case data if data.nonEmpty => Some(TransactionType(data.head))
        case _                     => None
      }
  }
}

// todo: (NODE-1915) Relevant name
trait HeadedTransactionParser[T <: Transaction] extends NewTransactionParser[T] {
  import NewTransactionParser._

  /** @return (offset, version, typeId) */
  def parseHeaderInfo(bytes: Array[Byte]): Try[(Int, TransactionVersion, TransactionType)]
  def parseBody(body: Array[Byte], version: TransactionVersion): Try[T]

  def parseBytes(bytes: Array[Byte]): Try[T] =
    for {
      (offset, parsedVersion, parsedTxType) <- parseHeaderInfo(bytes)
      _                                     <- expectedType(transactionType, parsedTxType)
      _                                     <- expectedVersion(parsedVersion, supportedVersions)
      tx                                    <- parseBody(bytes.drop(offset), parsedVersion)
    } yield tx
}

trait HardcodedV1Parser[T <: Transaction] extends HeadedTransactionParser[T] {
  import NewTransactionParser._

  val Version1: TransactionVersion = TransactionVersion(1)

  def parseBody(body: Array[Byte]): Try[T]

  override def parseHeaderInfo(bytes: Array[Byte]): Try[(Int, TransactionVersion, TransactionType)] =
    for {
      _ <- expectedMinSize(bytes, 1)
      txType = TransactionType(bytes.head)
      _ <- expectedType(TransactionType(bytes.head), transactionType)
    } yield (HardcodedV1Offset, Version1, txType)

  override def parseBody(body: Array[Byte], version: TransactionVersion): Try[T] =
    parseBody(body)

  override def supportedVersions: Set[TransactionVersion] = Set(Version1)
}

trait OneVersionParser[T <: Transaction] extends HeadedTransactionParser[T] {
  import NewTransactionParser._

  def version: TransactionVersion
  def parseBody(body: Array[Byte]): Try[T]

  override def parseHeaderInfo(bytes: Array[Byte]): Try[(Int, TransactionVersion, TransactionType)] =
    for {
      _ <- expectedMinSize(bytes, 2)
      txType    = TransactionType(bytes.head)
      txVersion = TransactionVersion(bytes.tail.head)
    } yield (OneVersionOffset, txVersion, txType)

  override def parseBody(body: Array[Byte], version: TransactionVersion): Try[T] =
    parseBody(body)

  override def supportedVersions: Set[TransactionVersion] = Set(version)
}

trait FallbackVersionParser[T <: Transaction] extends HeadedTransactionParser[T] {
  import NewTransactionParser._

  override def parseHeaderInfo(bytes: Array[Byte]): Try[(Int, TransactionVersion, TransactionType)] =
    bytes.headOption.fold[Try[(Int, TransactionVersion, TransactionType)]](
      Failure(new IllegalArgumentException("Can't find the significant byte: the buffer is empty"))
    ) { headByte =>
      if (headByte == 0)
        for {
          _ <- expectedMinSize(bytes, 3)
          _ <- Either.cond(bytes.head == 0, (), new IllegalArgumentException(s"Expected the '0' byte, but got '${bytes.head}'")).toTry
          txType    = TransactionType(bytes(1))
          txVersion = TransactionVersion(bytes(2))
        } yield (MultiVersionOffset, txVersion, txType)
      else
        for {
          _ <- expectedMinSize(bytes, 2)
          txType    = TransactionType(bytes.head)
          txVersion = TransactionVersion(1) // todo: (NODE-1915) always?
        } yield (HardcodedV1Offset, txVersion, txType)
    }
}
