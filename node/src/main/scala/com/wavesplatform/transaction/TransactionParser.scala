package com.wavesplatform.transaction

import cats.implicits._
import com.wavesplatform.transaction.description.{ByteEntity, ConstantByte, OneByte}

import scala.reflect.ClassTag
import scala.util.Try

trait TransactionManifest {
  type TransactionT <: Transaction

  def classTag: ClassTag[TransactionT]
  def typeId: Byte
  def supportedVersions: Set[Byte]
}

trait TransactionParserLite extends TransactionManifest {
  def parseBytes(bytes: Array[Byte]): Try[TransactionT]
}

trait TransactionBytesDescription { self: TransactionManifest =>
  /** Byte description of the header of the transaction */
  val byteHeaderDescription: ByteEntity[Unit]

  /**
    * Byte description of the transaction. Can be used for deserialization.
    *
    * Implementation example:
    * {{{
    *   val bytesTailDescription: ByteEntity[Transaction] =
    *   (
    *     OneByte(1, "Transaction type"),
    *     OneByte(2, "Version"),
    *     LongBytes(3, "Fee")
    *   ) mapN { case (txType, version, fee) => Transaction(txType, version, fee) }
    *
    *   // deserialization from buf: Array[Byte]
    *   val tx: Try[Transaction] = byteTailDescription.deserializeFromByteArray(buf)
    * }}}
    */
  val byteTailDescription: ByteEntity[TransactionT]

  /**
    * Returns index of byte entity in `byteTailDescription`
    * taking into account the last index in `byteHeaderDescription`
    */
  protected def tailIndex(index: Int): Int = byteHeaderDescription.index + index

  /**
    * Full byte description of the transaction (header + tail). Can be used for deserialization and generation of the documentation.
    *
    * Usage example:
    * {{{
    *   // generation of the documentation
    *   val txStringDocumentationForMD: String = byteDescription.getStringDocForMD
    * }}}
    */
  lazy val byteDescription: ByteEntity[TransactionT] = (byteHeaderDescription, byteTailDescription) mapN { case (_, tx) => tx }
}

object TransactionBytesDescription {
  trait HardcodedVersion1 { self: TransactionManifest with TransactionBytesDescription =>
    override val supportedVersions: Set[Byte] = Set(1)
    lazy val byteHeaderDescription: ByteEntity[Unit] = {
      ConstantByte(1, typeId, "Transaction type") map (_ => Unit)
    }
  }

  trait OneVersion { self: TransactionManifest with TransactionBytesDescription =>
    def version: Byte
    override def supportedVersions: Set[Byte] = Set(version)

    lazy val byteHeaderDescription: ByteEntity[Unit] = {
      (
        ConstantByte(1, value = typeId, name = "Transaction type"),
        ConstantByte(2, value = version, name = "Version")
      ) mapN ((_, _) => Unit)
    }
  }

  trait MultipleVersions { self: TransactionManifest with TransactionBytesDescription =>
    lazy val byteHeaderDescription: ByteEntity[Unit] = {
      (
        ConstantByte(1, value = 0, name = "Transaction multiple version mark"),
        ConstantByte(2, value = typeId, name = "Transaction type"),
        OneByte(3, "Version")
      ) mapN ((_, _, _) => Unit)
    }
  }
}

abstract class TransactionBytesDescriptionFor[M <: TransactionManifest](val manifest: M) extends TransactionManifest with TransactionBytesDescription {
  override final type TransactionT = manifest.TransactionT
  override def classTag: ClassTag[manifest.TransactionT] = manifest.classTag
  override def typeId: TxVersion = manifest.typeId
  override def supportedVersions: Set[TxVersion] = manifest.supportedVersions
}

trait TransactionParser extends TransactionParserLite with TransactionBytesDescription {
  override def parseBytes(bytes: Array[Byte]): Try[TransactionT] =
    parseHeader(bytes) flatMap (offset => parseTail(bytes drop offset))

  /** @return offset */
  protected def parseHeader(bytes: Array[Byte]): Try[Int]
  protected def parseTail(bytes: Array[Byte]): Try[TransactionT]
}

object TransactionParser {

  trait HardcodedVersion1 extends TransactionBytesDescription.HardcodedVersion1 { self: TransactionParser =>
    override protected def parseHeader(bytes: Array[Byte]): Try[Int] = Try {
      if (bytes.length < 1) throw new IllegalArgumentException(s"The buffer is too small, it has ${bytes.length} elements")
      val parsedTypeId = bytes.head
      if (parsedTypeId != typeId) throw new IllegalArgumentException(s"Expected type of transaction '$typeId', but got '$parsedTypeId'")
      1
    }
  }

  trait OneVersion extends TransactionBytesDescription.OneVersion { self: TransactionParser =>
    override protected def parseHeader(bytes: Array[Byte]): Try[Int] = Try {
      if (bytes.length < 2) throw new IllegalArgumentException(s"The buffer is too small, it has ${bytes.length} elements")
      val Array(parsedTypeId, parsedVersion) = bytes.take(2)

      if (parsedTypeId != typeId) throw new IllegalArgumentException(s"Expected type of transaction '$typeId', but got '$parsedTypeId'")
      if (!supportedVersions.contains(parsedVersion))
        throw new IllegalArgumentException(s"Expected version of transaction: $version, but got '$parsedVersion'")

      2
    }
  }

  trait MultipleVersions extends TransactionBytesDescription.MultipleVersions { self: TransactionParser =>

    override protected def parseHeader(bytes: Array[Byte]): Try[Int] = Try {
      if (bytes.length < 3) throw new IllegalArgumentException(s"The buffer is too small, it has ${bytes.length} elements")
      val Array(parsedMark, parsedTypeId, parsedVersion) = bytes.take(3)

      if (parsedMark != 0) throw new IllegalArgumentException(s"Expected the '0' byte, but got '$parsedMark'")
      if (parsedTypeId != typeId) throw new IllegalArgumentException(s"Expected type of transaction '$typeId', but got '$parsedTypeId'")
      if (!supportedVersions.contains(parsedVersion))
        throw new IllegalArgumentException(s"Expected version of transaction ${supportedVersions.mkString(", ")}, but got '$parsedVersion'")

      3
    }
  }
}

abstract class TransactionParserFor[T <: Transaction](implicit override val classTag: ClassTag[T]) extends TransactionParser {
  override type TransactionT = T
}
