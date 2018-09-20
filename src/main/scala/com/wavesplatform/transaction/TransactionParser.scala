package com.wavesplatform.transaction

import scala.reflect.ClassTag
import scala.util.Try

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
  }

}

abstract class TransactionParserFor[T <: Transaction](implicit override val classTag: ClassTag[T]) extends TransactionParser {
  override type TransactionT = T
}
