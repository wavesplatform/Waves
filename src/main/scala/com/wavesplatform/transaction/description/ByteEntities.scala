package com.wavesplatform.transaction.description

import com.google.common.primitives.{Ints, Longs, Shorts}
import com.wavesplatform.account.{Address, AddressOrAlias, Alias, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto.{KeyLength, SignatureLength}
import com.wavesplatform.lang.v1.Serde
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.serialization.Deser
import com.wavesplatform.state.DataEntry
import com.wavesplatform.transaction.ValidationError.Validation
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.smart.ContractInvocationTransaction.Payment
import com.wavesplatform.transaction.smart.script.{Script, ScriptReader}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer

import scala.util.{Failure, Success, Try}

/**
  * Represents description of the byte entity
  * Field `additionalInfo` can be used for specifying of the repeating byte entities
  */
case class ByteEntityDescription(index: String, name: String, tpe: String, length: String, additionalInfo: String = "")

/**
  * Describes byte representation of the different types. Composition of Byte Entities can be used for deserialization
  * and generation of the documentation of the complex data structures, such as transactions, messages, orders, etc
  */
sealed trait ByteEntity[T] { self =>

  private[description] val ByteType           = "Byte"
  private[description] val BooleanType        = "Boolean"
  private[description] val ShortType          = "Short"
  private[description] val IntType            = "Int"
  private[description] val LongType           = "Long"
  private[description] val ByteArrayType      = "Array[Byte]"
  private[description] val ByteStrType        = s"ByteStr ($ByteArrayType)"
  private[description] val AddressType        = "Address"
  private[description] val AliasType          = "Alias"
  private[description] val AddressOrAliasType = "Address or Alias"
  private[description] val OrderV1Type        = "OrderV1"
  private[description] val OrderType          = "Order"

  /** Index of the byte entity. In case of composition of byte entities returns index of the last one */
  val index: Int

  private[description] def generateDoc(): Seq[ByteEntityDescription]

  private[description] def deserialize(buf: Array[Byte], offset: Int): Try[(T, Int)]

  def deserializeFromByteArray(buf: Array[Byte]): Try[T] = deserialize(buf, 0) map { case (value, _) => value }

  def ~[U](other: ByteEntity[U]): ByteEntity[(T, U)] = Composition(this, other)

  def map[U](f: T => U): ByteEntity[U] = new ByteEntity[U] {

    val index: Int = self.index

    def generateDoc(): Seq[ByteEntityDescription] = self.generateDoc()

    def deserialize(buf: Array[Byte], offset: Int): Try[(U, Int)] = self.deserialize(buf, offset).map { case (t, o) => f(t) -> o }
  }

  /** Generates documentation as a string */
  def getStringDoc: String = {

    val docs = generateDoc()

    val indicesMaxLength = docs.map(_.index.length).max
    val namesMaxLength   = docs.map(_.name.length).max
    val typesMaxLength   = docs.map(_.tpe.length).max
    val lengthsMaxLength = docs.map(_.length.length).max

    docs
      .map {
        case ByteEntityDescription(idx, name, tpe, length, additionalInfo) =>
          "Index: " + idx.padTo(indicesMaxLength, " ").mkString ++
            " Name: " + name.padTo(namesMaxLength, " ").mkString ++
            " Type: " + tpe.padTo(typesMaxLength, " ").mkString ++
            " Length: " + length.padTo(lengthsMaxLength, " ").mkString ++
            additionalInfo
      }
      .mkString("\n")
  }

  /** Generates documentation ready for pasting into .md files */
  def getStringDocForMD: String = {

    val docs = generateDoc()

    docs
      .map {
        case ByteEntityDescription(idx, name, tpe, length, additionalInfo) =>
          s"| $idx | $name | $tpe | $length $additionalInfo\n"
            .replace("...", "| ... | ... | ... | ... |")
            .replace("(", "\\(")
            .replace(")", "\\)")
            .replace("*", "\\*")
      }
      .foldLeft("""| \# | Field name | Type | Length |""" + "\n| --- | --- | --- | --- |\n")(_ + _)
  }
}

case class ConstantByte(index: Int, value: Byte, name: String) extends ByteEntity[Byte] {

  def generateDoc(): Seq[ByteEntityDescription] = Seq(ByteEntityDescription(index.toString, name, s"$ByteType (constant, value = $value)", "1"))

  def deserialize(buf: Array[Byte], offset: Int): Try[(Byte, Int)] = {
    Try { value -> (offset + 1) }
  }
}

case class OneByte(index: Int, name: String) extends ByteEntity[Byte] {

  def generateDoc(): Seq[ByteEntityDescription] = Seq(ByteEntityDescription(index.toString, name, ByteType, "1"))

  def deserialize(buf: Array[Byte], offset: Int): Try[(Byte, Int)] = {
    Try { buf(offset) -> (offset + 1) }
  }
}

case class LongBytes(index: Int, name: String) extends ByteEntity[Long] {

  def generateDoc(): Seq[ByteEntityDescription] = Seq(ByteEntityDescription(index.toString, name, LongType, "8"))

  def deserialize(buf: Array[Byte], offset: Int): Try[(Long, Int)] = {
    Try { Longs.fromByteArray(buf.slice(offset, offset + 8)) -> (offset + 8) }
  }
}

case class IntBytes(index: Int, name: String) extends ByteEntity[Int] {

  def generateDoc(): Seq[ByteEntityDescription] = Seq(ByteEntityDescription(index.toString, name, IntType, "4"))

  def deserialize(buf: Array[Byte], offset: Int): Try[(Int, Int)] = {
    Try { Ints.fromByteArray(buf.slice(offset, offset + 4)) -> (offset + 4) }
  }
}

case class BooleanByte(index: Int, name: String) extends ByteEntity[Boolean] {

  def generateDoc(): Seq[ByteEntityDescription] = Seq(ByteEntityDescription(index.toString, name, BooleanType, "1"))

  def deserialize(buf: Array[Byte], offset: Int): Try[(Boolean, Int)] = {
    Try { (buf(offset) == 1) -> (offset + 1) }
  }
}

case class BytesArrayDefinedLength(index: Int, name: String, length: Int) extends ByteEntity[Array[Byte]] {

  def generateDoc(): Seq[ByteEntityDescription] = Seq(ByteEntityDescription(index.toString, name, ByteArrayType, length.toString))

  def deserialize(buf: Array[Byte], offset: Int): Try[(Array[Byte], Int)] = {
    Try { buf.slice(offset, offset + length) -> (offset + length) }
  }
}

case class BytesArrayUndefinedLength(index: Int, name: String) extends ByteEntity[Array[Byte]] {

  def generateDoc(): Seq[ByteEntityDescription] = {
    Seq(
      ByteEntityDescription(s"$index.1", s"$name length (N)", ShortType, "2"),
      ByteEntityDescription(s"$index.2", name, ByteArrayType, "N")
    )
  }

  def deserialize(buf: Array[Byte], offset: Int): Try[(Array[Byte], Int)] = {
    Try {
      val length                 = Shorts.fromByteArray(buf.slice(offset, offset + 2))
      val (arrayStart, arrayEnd) = (offset + 2, offset + 2 + length)
      buf.slice(arrayStart, arrayEnd) -> arrayEnd
    }
  }
}

case class ByteStrDefinedLength(index: Int, name: String, length: Int) extends ByteEntity[ByteStr] {

  def generateDoc(): Seq[ByteEntityDescription] = {
    Seq(ByteEntityDescription(index.toString, name, ByteStrType, length.toString))
  }

  def deserialize(buf: Array[Byte], offset: Int): Try[(ByteStr, Int)] = {
    Try { ByteStr(buf.slice(offset, offset + length)) -> (offset + length) }
  }
}

case class PublicKeyAccountBytes(index: Int, name: String) extends ByteEntity[PublicKeyAccount] {

  def generateDoc(): Seq[ByteEntityDescription] =
    Seq(ByteEntityDescription(index.toString, name, s"PublicKeyAccount ($ByteArrayType)", KeyLength.toString))

  def deserialize(buf: Array[Byte], offset: Int): Try[(PublicKeyAccount, Int)] = {
    Try { PublicKeyAccount(buf.slice(offset, offset + KeyLength)) -> (offset + KeyLength) }
  }
}

case class SignatureBytes(index: Int, name: String) extends ByteEntity[ByteStr] {

  def generateDoc(): Seq[ByteEntityDescription] = Seq(ByteEntityDescription(index.toString, name, ByteStrType, SignatureLength.toString))

  def deserialize(buf: Array[Byte], offset: Int): Try[(ByteStr, Int)] = {
    Try { ByteStr(buf.slice(offset, offset + SignatureLength)) -> (offset + SignatureLength) }
  }
}

case class SponsorFeeOptionLongBytes(index: Int, name: String, additionalInfo: String = "") extends ByteEntity[Option[Long]] {

  def generateDoc(): Seq[ByteEntityDescription] = {
    Seq(ByteEntityDescription(index.toString, name, "Option[Long]", LongType, additionalInfo))
  }

  def deserialize(buf: Array[Byte], offset: Int): Try[(Option[Long], Int)] = {
    Try { Option(Longs.fromByteArray(buf.slice(offset, offset + 8))).filter(_ != 0) -> (offset + 8) }
  }
}

case class OptionAssetIdBytes(index: Int, name: String) extends ByteEntity[Option[AssetId]] {

  import com.wavesplatform.transaction.AssetIdLength

  def generateDoc(): Seq[ByteEntityDescription] = {
    Seq(
      ByteEntityDescription(s"$index.1", s"$name flag (0 - Waves, 1 - asset)", ByteType, "1"),
      ByteEntityDescription(s"$index.2", name, "AssetId (ByteStr = Array[Byte])", s"0/$AssetIdLength")
    )
  }

  def deserialize(buf: Array[Byte], offset: Int): Try[(Option[AssetId], Int)] = {
    Try {
      if (buf(offset) == (1: Byte)) Some(ByteStr(buf.slice(offset + 1, offset + 1 + AssetIdLength))) -> (offset + 1 + AssetIdLength)
      else (None, offset + 1)
    }
  }
}

case class AddressBytes(index: Int, name: String) extends ByteEntity[Address] {

  def generateDoc(): Seq[ByteEntityDescription] = {
    Seq(ByteEntityDescription(s"$index", s"$name", AddressType, s"${Address.AddressLength}"))
  }

  def deserialize(buf: Array[Byte], offset: Int): Try[(Address, Int)] = {
    Try {

      val recipientBytes = java.util.Arrays.copyOfRange(buf, offset, offset + Address.AddressLength)
      val recipient      = Address.fromBytes(recipientBytes).explicitGet()

      recipient -> (offset + Address.AddressLength)
    }
  }
}

case class AliasBytes(index: Int, name: String) extends ByteEntity[Alias] {

  def generateDoc(): Seq[ByteEntityDescription] = {
    Seq(
      ByteEntityDescription(s"${index.toString}.1", s"$name length (A)", ShortType, "2"),
      ByteEntityDescription(s"${index.toString}.2", s"$name", AliasType, "A")
    )
  }

  def deserialize(buf: Array[Byte], offset: Int): Try[(Alias, Int)] = {
    val aliasLength = Shorts.fromByteArray(buf.slice(offset, offset + 2))
    Alias
      .fromBytes(buf.slice(offset + 2, offset + 2 + aliasLength))
      .map(alias => alias -> (offset + 2 + aliasLength))
      .fold(err => Failure(new Exception(err.toString)), Success.apply)
  }
}

case class AddressOrAliasBytes(index: Int, name: String) extends ByteEntity[AddressOrAlias] {

  def generateDoc(): Seq[ByteEntityDescription] =
    Seq(ByteEntityDescription(index.toString, name, AddressOrAliasType, "depends on first byte (1 - Address, 2 - Alias)"))

  def deserialize(buf: Array[Byte], offset: Int): Try[(AddressOrAlias, Int)] = {
    Try { AddressOrAlias.fromBytes(buf, offset).explicitGet() }
  }
}

case class ProofsBytes(index: Int) extends ByteEntity[Proofs] {

  def generateDoc(): Seq[ByteEntityDescription] = {
    Seq(
      ByteEntityDescription(s"$index.1", "Proofs version (0x01)", ByteType, "1"),
      ByteEntityDescription(s"$index.2", "Proofs count", ShortType, "2"),
      ByteEntityDescription(s"$index.3", "Proof 1 length (P1)", ShortType, "2"),
      ByteEntityDescription(s"$index.4", "Proof 1", ByteStrType, "P1"),
      ByteEntityDescription(s"$index.5", "Proof 2 length (P2)", ShortType, "2"),
      ByteEntityDescription(s"$index.6", "Proof 2 ", ByteStrType, "P2", "\n...")
    )
  }

  def deserialize(buf: Array[Byte], offset: Int): Try[(Proofs, Int)] = {
    Try { Proofs.fromBytes(buf.drop(offset)).map(p => p -> (offset + p.bytes.value.length)).explicitGet() }
  }
}

case class TransfersBytes(index: Int) extends ByteEntity[List[ParsedTransfer]] {

  import cats.implicits._

  private def readTransfer(buf: Array[Byte], offset: Int): (Validation[ParsedTransfer], Int) = {
    AddressOrAlias.fromBytes(buf, offset) match {
      case Right((addressOrAlias, ofs)) =>
        val amount = Longs.fromByteArray(buf.slice(ofs, ofs + 8))
        Right[ValidationError, ParsedTransfer](ParsedTransfer(addressOrAlias, amount)) -> (ofs + 8)
      case Left(validationError) => Left(validationError) -> offset
    }
  }

  def generateDoc(): Seq[ByteEntityDescription] = {
    Seq(
      ByteEntityDescription(s"$index.1", "Number of transfers", ShortType, "2"),
      ByteEntityDescription(s"$index.2", "Address or alias for transfer 1", AddressOrAliasType, "depends on first byte (1 - Address, 2 - Alias)"),
      ByteEntityDescription(s"$index.3", "Amount for transfer 1", LongType, "8"),
      ByteEntityDescription(s"$index.4", "Address or alias for transfer 2", AddressOrAliasType, "depends on first byte (1 - Address, 2 - Alias)"),
      ByteEntityDescription(s"$index.5", "Amount for transfer 2", LongType, "8", "\n...")
    )
  }

  def deserialize(buf: Array[Byte], offset: Int): Try[(List[ParsedTransfer], Int)] = {
    Try {

      val transferCount = Shorts.fromByteArray(buf.slice(offset, offset + 2))

      val transfersList: List[(Validation[ParsedTransfer], Int)] =
        List.iterate(readTransfer(buf, offset + 2), transferCount) { case (_, offst) => readTransfer(buf, offst) }

      val resultOffset = transfersList.lastOption.map(_._2).getOrElse(offset + 2)
      val resultList   = transfersList.map { case (ei, _) => ei }.sequence.explicitGet()

      resultList -> resultOffset
    }
  }

}

case class OrderBytes(index: Int, name: String) extends ByteEntity[Order] {

  def generateDoc(): Seq[ByteEntityDescription] = {
    Seq(
      ByteEntityDescription(s"${index.toString}.1", s"$name size (N)", IntType, "4"),
      ByteEntityDescription(s"${index.toString}.2", s"$name version mark", ByteType, "1 (version 1) / 0 (version 2)"),
      ByteEntityDescription(s"${index.toString}.3", s"$name", OrderType, "N")
    )
  }

  def deserialize(buf: Array[Byte], offset: Int): Try[(Order, Int)] = {
    Try {

      val orderSize = Ints.fromByteArray(buf.slice(offset, offset + 4))
      val orderMark = buf(offset + 4)

      if (orderMark == 1)
        OrderV1.parseBytes(buf.drop(offset + 5)).map(order => order -> (offset + 5 + orderSize))
      else
        OrderV2.parseBytes(buf.drop(offset + 4)).map(order => order -> (offset + 4 + orderSize))

    }.flatten
  }
}

case class OrderV1Bytes(index: Int, name: String, length: String) extends ByteEntity[OrderV1] {

  def generateDoc(): Seq[ByteEntityDescription] = Seq(ByteEntityDescription(index.toString, name, OrderV1Type, s"$length"))

  def deserialize(buf: Array[Byte], offset: Int): Try[(OrderV1, Int)] = {
    OrderV1.parseBytes(buf.drop(offset)).map { order =>
      order -> (offset + order.bytes.value.length)
    }
  }
}

case class OptionScriptBytes(index: Int, name: String) extends ByteEntity[Option[Script]] {

  def generateDoc(): Seq[ByteEntityDescription] = {
    Seq(
      ByteEntityDescription(s"${index.toString}.1", s"$name existence flag", ByteType, "1"),
      ByteEntityDescription(s"${index.toString}.2", s"$name", "Script", "S")
    )
  }

  def deserialize(buf: Array[Byte], offset: Int): Try[(Option[Script], Int)] = {
    Try {

      val (scriptOptEi: Option[Either[ValidationError.ScriptParseError, Script]], scriptEnd) =
        Deser.parseOption(buf, offset)(ScriptReader.fromBytes)

      val scriptEiOpt: Either[ValidationError.ScriptParseError, Option[Script]] = scriptOptEi match {
        case None            => Right(None)
        case Some(Right(sc)) => Right(Some(sc))
        case Some(Left(err)) => Left(err)
      }

      scriptEiOpt.map(_ -> scriptEnd).explicitGet()
    }
  }
}

case class OptionPaymentBytes(index: Int, name: String) extends ByteEntity[Option[Payment]] {

  def generateDoc(): Seq[ByteEntityDescription] = {
    Seq(ByteEntityDescription(index.toString, name, "Option[Payment]", "OP"))
  }

  def deserialize(buf: Array[Byte], offset: Int): Try[(Option[Payment], Int)] = {

    Try {
      val (payment: Option[(Option[AssetId], Long)], ofs) =
        Deser.parseOption(buf, offset)(arr => {
          val amt: Long                        = Longs.fromByteArray(arr.take(8))
          val (maybeAsset: Option[AssetId], _) = Deser.parseOption(arr, 8)(ByteStr.apply)
          (maybeAsset, amt)
        })

      payment.map(p => Payment(p._2, p._1)) -> ofs
    }
  }
}

case class ListDataEntryBytes(index: Int) extends ByteEntity[List[DataEntry[_]]] {

  def generateDoc(): Seq[ByteEntityDescription] = {
    Seq(
      ByteEntityDescription(s"${index.toString}.1", "Data entries count", ShortType, "2"),
      ByteEntityDescription(s"${index.toString}.2", "Key 1 length (K1)", ShortType, "2"),
      ByteEntityDescription(s"${index.toString}.3", "Key 1 bytes", "UTF-8 encoded", "K1"),
      ByteEntityDescription(s"${index.toString}.4", "Value 1 type (0 = integer, 1 = boolean, 2 = binary array, 3 = string)", ByteType, "1"),
      ByteEntityDescription(s"${index.toString}.5", "Value 1 bytes", "Value 1 type", "depends on value type", "\n...")
    )
  }

  def deserialize(buf: Array[Byte], offset: Int): Try[(List[DataEntry[_]], Int)] = {
    Try {

      val entryCount = Shorts.fromByteArray(buf.slice(offset, offset + 2))

      if (entryCount > 0) {
        val parsed = List.iterate(DataEntry.parse(buf, offset + 2), entryCount) { case (_, p) => DataEntry.parse(buf, p) }
        parsed.map(_._1) -> parsed.last._2
      } else
        List.empty -> (offset + 2)
    }
  }
}

case class FunctionCallBytes(index: Int, name: String) extends ByteEntity[Terms.FUNCTION_CALL] {

  def generateDoc(): Seq[ByteEntityDescription] = {
    Seq(ByteEntityDescription(index.toString, name, "EXPR", "F"))
  }

  def deserialize(buf: Array[Byte], offset: Int): Try[(Terms.FUNCTION_CALL, Int)] = {
    Try {
      val (expr, remaining) = Serde.deserialize(buf.drop(offset), all = false).explicitGet()
      expr.asInstanceOf[FUNCTION_CALL] -> (buf.length - remaining)
    }
  }
}

case class Composition[T1, T2](e1: ByteEntity[T1], e2: ByteEntity[T2]) extends ByteEntity[(T1, T2)] {

  val index: Int = e2.index // use last index in composition

  def generateDoc(): Seq[ByteEntityDescription] = e1.generateDoc() ++ e2.generateDoc()

  def deserialize(buf: Array[Byte], offset: Int): Try[((T1, T2), Int)] =
    for {
      (value1, offset1) <- e1.deserialize(buf, offset)
      (value2, offset2) <- e2.deserialize(buf, offset1)
    } yield ((value1, value2), offset2)
}
