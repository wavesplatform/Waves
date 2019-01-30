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

import scala.util.Try

/** Represents description of the byte entity */
case class ByteEntityDescription(index: String, name: String, tpe: String, length: String, additionalInfo: String = "")

/**
  * Describes byte representation of the different types. Composition of Byte Entities can be used for deserialization
  * and generation of the documentation of the complex data structures, such as transactions, messages, orders and so on
  */
sealed trait ByteEntity[T] { self =>

  val ByteType           = "Byte"
  val BooleanType        = "Boolean"
  val ShortType          = "Short"
  val IntType            = "Int"
  val LongType           = "Long"
  val ByteArrayType      = "Array[Byte]"
  val ByteStrType        = s"ByteStr ($ByteArrayType)"
  val AddressType        = "Address"
  val AliasType          = "Alias"
  val AddressOrAliasType = "Address or Alias"
  val OrderV1Type        = "OrderV1"
  val OrderType          = "Order"

  val index: Int

  def generateDoc(): Seq[ByteEntityDescription]

  def deserialize(buf: Array[Byte], offset: Int): Option[(T, Int)]

  def deserializeFromByteArray(buf: Array[Byte]): Option[T] = deserialize(buf, 0) map { case (value, _) ⇒ value }

  def ~[U](other: ByteEntity[U]): ByteEntity[(T, U)] = Composition(this, other)

  def |(other: ByteEntity[T]): ByteEntity[T] = new ByteEntity[T] {

    val index: Int = self.index

    def generateDoc(): Seq[ByteEntityDescription] = self.generateDoc() ++ other.generateDoc()

    def deserialize(buf: Array[Byte], offset: Int): Option[(T, Int)] =
      self.deserialize(buf, offset).orElse(other.deserialize(buf, offset))
  }

  def map[U](f: T => U): ByteEntity[U] = new ByteEntity[U] {

    val index: Int = self.index

    def generateDoc(): Seq[ByteEntityDescription] = self.generateDoc()

    def deserialize(buf: Array[Byte], offset: Int): Option[(U, Int)] = self.deserialize(buf, offset).map { case (t, o) => f(t) -> o }
  }

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

  def deserialize(buf: Array[Byte], offset: Int): Option[(Byte, Int)] = Some(value -> (offset + 1))
}

case class OneByte(index: Int, name: String) extends ByteEntity[Byte] {

  def generateDoc(): Seq[ByteEntityDescription] = Seq(ByteEntityDescription(index.toString, name, ByteType, "1"))

  def deserialize(buf: Array[Byte], offset: Int): Option[(Byte, Int)] = {
    Try { buf(offset) -> (offset + 1) }.toOption
  }
}

case class LongBytes(index: Int, name: String) extends ByteEntity[Long] {

  def generateDoc(): Seq[ByteEntityDescription] = Seq(ByteEntityDescription(index.toString, name, LongType, "8"))

  def deserialize(buf: Array[Byte], offset: Int): Option[(Long, Int)] = {
    Try { Longs.fromByteArray(buf.slice(offset, offset + 8)) -> (offset + 8) }.toOption
  }
}

case class IntBytes(index: Int, name: String) extends ByteEntity[Int] {

  def generateDoc(): Seq[ByteEntityDescription] = Seq(ByteEntityDescription(index.toString, name, IntType, "4"))

  def deserialize(buf: Array[Byte], offset: Int): Option[(Int, Int)] = {
    Try { Ints.fromByteArray(buf.slice(offset, offset + 4)) -> (offset + 4) }.toOption
  }
}

case class BooleanByte(index: Int, name: String) extends ByteEntity[Boolean] {

  def generateDoc(): Seq[ByteEntityDescription] = Seq(ByteEntityDescription(index.toString, name, BooleanType, "1"))

  def deserialize(buf: Array[Byte], offset: Int): Option[(Boolean, Int)] = {
    Try { (buf(offset) == 1) -> (offset + 1) }.toOption
  }
}

case class BytesArrayDefinedLength(index: Int, name: String, length: Int) extends ByteEntity[Array[Byte]] {

  def generateDoc(): Seq[ByteEntityDescription] = Seq(ByteEntityDescription(index.toString, name, ByteArrayType, length.toString))

  def deserialize(buf: Array[Byte], offset: Int): Option[(Array[Byte], Int)] = {
    Try { buf.slice(offset, offset + length) -> (offset + length) }.toOption
  }
}

case class BytesArrayUndefinedLength(index: Int, name: String) extends ByteEntity[Array[Byte]] {

  def generateDoc(): Seq[ByteEntityDescription] = {
    Seq(
      ByteEntityDescription(s"$index.1", s"$name length (N)", ShortType, "2"),
      ByteEntityDescription(s"$index.2", name, ByteArrayType, "N")
    )
  }

  def deserialize(buf: Array[Byte], offset: Int): Option[(Array[Byte], Int)] = {
    Try {
      val length                 = Shorts.fromByteArray(buf.slice(offset, offset + 2))
      val (arrayStart, arrayEnd) = (offset + 2, offset + 2 + length)
      buf.slice(arrayStart, arrayEnd) -> arrayEnd
    }.toOption
  }

}

case class ByteStrDefinedLength(index: Int, name: String, length: Int) extends ByteEntity[ByteStr] {

  def generateDoc(): Seq[ByteEntityDescription] = {
    Seq(ByteEntityDescription(index.toString, name, ByteStrType, length.toString))
  }

  def deserialize(buf: Array[Byte], offset: Int): Option[(ByteStr, Int)] = {
    Try { ByteStr(buf.slice(offset, length)) -> (offset + length) }.toOption
  }
}

case class PublicKeyAccountBytes(index: Int, name: String) extends ByteEntity[PublicKeyAccount] {

  def generateDoc(): Seq[ByteEntityDescription] =
    Seq(ByteEntityDescription(index.toString, name, s"PublicKeyAccount ($ByteArrayType)", KeyLength.toString))

  def deserialize(buf: Array[Byte], offset: Int): Option[(PublicKeyAccount, Int)] = {
    Try { PublicKeyAccount(buf.slice(offset, offset + KeyLength)) -> (offset + KeyLength) }.toOption
  }
}

case class SignatureBytes(index: Int, name: String) extends ByteEntity[ByteStr] {

  def generateDoc(): Seq[ByteEntityDescription] = Seq(ByteEntityDescription(index.toString, name, ByteStrType, SignatureLength.toString))

  def deserialize(buf: Array[Byte], offset: Int): Option[(ByteStr, Int)] = {
    Try { ByteStr(buf.slice(offset, offset + SignatureLength)) -> (offset + SignatureLength) }.toOption
  }
}

case class OptionLongBytes(index: Int, name: String, additionalInfo: String = "") extends ByteEntity[Option[Long]] {

  def generateDoc(): Seq[ByteEntityDescription] = {
    Seq(ByteEntityDescription(index.toString, name, "Option[Long]", LongType, additionalInfo))
  }

  def deserialize(buf: Array[Byte], offset: Int): Option[(Option[Long], Int)] = {
    Try { Option(Longs.fromByteArray(buf.slice(offset, offset + 8))).filter(_ != 0) -> (offset + 8) }.toOption
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

  def deserialize(buf: Array[Byte], offset: Int): Option[(Option[AssetId], Int)] = {
    Try {
      if (buf(offset) == (1: Byte)) Some(ByteStr(buf.slice(offset + 1, offset + 1 + AssetIdLength))) -> (offset + 1 + AssetIdLength)
      else (None, offset + 1)
    }.toOption
  }
}

case class AddressBytes(index: Int, name: String) extends ByteEntity[Address] {

  def generateDoc(): Seq[ByteEntityDescription] = {
    Seq(ByteEntityDescription(s"$index", s"$name", AddressType, s"${Address.AddressLength}"))
  }

  def deserialize(buf: Array[Byte], offset: Int): Option[(Address, Int)] = {
    Try {

      val recipientBytes = java.util.Arrays.copyOfRange(buf, offset, offset + Address.AddressLength)
      val recepient      = Address.fromBytes(recipientBytes).explicitGet()

      recepient -> (offset + Address.AddressLength)

    }.toOption
  }
}

case class AliasBytes(index: Int, name: String) extends ByteEntity[Alias] {

  def generateDoc(): Seq[ByteEntityDescription] = {
    Seq(
      ByteEntityDescription(s"${index.toString}.1", s"$name length (N)", ShortType, "2"),
      ByteEntityDescription(s"${index.toString}.2", s"$name", AliasType, "N")
    )
  }

  def deserialize(buf: Array[Byte], offset: Int): Option[(Alias, Int)] = {
    Alias.fromBytes(buf).toOption.map { alias =>
      alias -> (offset + alias.bytes.arr.length)
    }
  }
}

case class AddressOrAliasBytes(index: Int, name: String) extends ByteEntity[AddressOrAlias] {

  def generateDoc(): Seq[ByteEntityDescription] =
    Seq(ByteEntityDescription(index.toString, name, AddressOrAliasType, "depends on first byte (1 - Address, 2 - Alias)"))

  def deserialize(buf: Array[Byte], offset: Int): Option[(AddressOrAlias, Int)] = {
    AddressOrAlias.fromBytes(buf, offset).toOption
  }
}

case class ProofsBytes(index: Int) extends ByteEntity[Proofs] {

  def generateDoc(): Seq[ByteEntityDescription] = {
    Seq(
      ByteEntityDescription(s"$index.1", "Proofs version (0x01)", ByteType, "1"),
      ByteEntityDescription(s"$index.2", "Proofs count", ShortType, "2"),
      ByteEntityDescription(s"$index.3", "Proof 1 length (N)", ShortType, "2"),
      ByteEntityDescription(s"$index.4", "Proof 1", ByteStrType, "N"),
      ByteEntityDescription(s"$index.5", "Proof 2 length (M)", ShortType, "2"),
      ByteEntityDescription(s"$index.6", "Proof 2 ", ByteStrType, "M", "\n...")
    )
  }

  def deserialize(buf: Array[Byte], offset: Int): Option[(Proofs, Int)] = {
    Proofs.fromBytes(buf.drop(offset)).toOption.map { p =>
      p -> (offset + p.bytes.value.length)
    }
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

  def deserialize(buf: Array[Byte], offset: Int): Option[(List[ParsedTransfer], Int)] = {

    Try {

      val transferCount = Shorts.fromByteArray(buf.slice(offset, offset + 2))

      val transfersList: List[(Validation[ParsedTransfer], Int)] =
        List.iterate(readTransfer(buf, offset + 2), transferCount) { case (_, offst) => readTransfer(buf, offst) }

      val resultOffset = transfersList.lastOption.map(_._2).getOrElse(offset + 2)
      val resultList   = transfersList.map { case (ei, _) => ei }.sequence.right.get

      resultList -> resultOffset
    }

  }.toOption

}

case class OrderBytes(index: Int, name: String) extends ByteEntity[Order] {

  def generateDoc(): Seq[ByteEntityDescription] = {
    Seq(
      ByteEntityDescription(s"${index.toString}.1", s"$name size (N)", IntType, "4"),
      ByteEntityDescription(s"${index.toString}.2", s"$name version mark", ByteType, "1 (version 1) / 0 (version 2)"),
      ByteEntityDescription(s"${index.toString}.3", s"$name", OrderType, "N")
    )
  }

  def deserialize(buf: Array[Byte], offset: Int): Option[(Order, Int)] = {
    Try {

      val orderSize = Ints.fromByteArray(buf.slice(offset, offset + 4))
      val orderMark = buf(offset + 4)

      if (orderMark == 1)
        OrderV1.parseBytes(buf.drop(5)).map(order => order -> (offset + 5 + orderSize))
      else
        OrderV2.parseBytes(buf.drop(4)).map(order => order -> (offset + 4 + orderSize))

    }.flatten.toOption
  }
}

case class OrderV1Bytes(index: Int, name: String, length: String) extends ByteEntity[OrderV1] {

  def generateDoc(): Seq[ByteEntityDescription] = Seq(ByteEntityDescription(index.toString, name, OrderV1Type, s"$length"))

  def deserialize(buf: Array[Byte], offset: Int): Option[(OrderV1, Int)] = {
    OrderV1
      .parseBytes(buf)
      .map { order =>
        order -> (offset + order.bytes.value.length)
      }
      .toOption
  }
}

case class OptionScriptBytes(index: Int, name: String) extends ByteEntity[Option[Script]] {

  def generateDoc(): Seq[ByteEntityDescription] = {
    Seq(
      ByteEntityDescription(s"${index.toString}.1", s"$name existence flag", ByteType, "1"),
      ByteEntityDescription(s"${index.toString}.2", s"$name", "Script", "S")
    )
  }

  def deserialize(buf: Array[Byte], offset: Int): Option[(Option[Script], Int)] = {

    val (scriptOptEi: Option[Either[ValidationError.ScriptParseError, Script]], scriptEnd) =
      Deser.parseOption(buf, offset)(ScriptReader.fromBytes)

    val scriptEiOpt: Either[ValidationError.ScriptParseError, Option[Script]] = scriptOptEi match {
      case None            => Right(None)
      case Some(Right(sc)) => Right(Some(sc))
      case Some(Left(err)) => Left(err)
    }

    scriptEiOpt.toOption.map(_ -> scriptEnd)
  }
}

case class OptionPaymentBytes(index: Int, name: String) extends ByteEntity[Option[Payment]] {

  def generateDoc(): Seq[ByteEntityDescription] = {
    Seq(ByteEntityDescription(index.toString, name, "Option[Payment]", "N"))
  }

  def deserialize(buf: Array[Byte], offset: Int): Option[(Option[Payment], Int)] = {

    Try {
      val (payment: Option[(Option[AssetId], Long)], ofs) = Deser.parseOption(buf, offset)(arr => {
        val amt: Long                        = Longs.fromByteArray(arr.take(8))
        val (maybeAsset: Option[AssetId], _) = Deser.parseOption(arr, 8)(ByteStr(_))
        (maybeAsset, amt)
      })

      payment.map(p => Payment(p._2, p._1)) -> ofs
    }.toOption
  }
}

case class ListDataEntryBytes(index: Int) extends ByteEntity[List[DataEntry[_]]] {

  def generateDoc(): Seq[ByteEntityDescription] = {
    Seq(
      ByteEntityDescription(s"${index.toString}.1", "Data entries count", ShortType, "2"),
      ByteEntityDescription(s"${index.toString}.2", "Key 1 length (N)", ShortType, "2"),
      ByteEntityDescription(s"${index.toString}.3", "Key 1 bytes", "UTF-8 encoded", "N"),
      ByteEntityDescription(s"${index.toString}.4", "Value 1 type (0 = integer, 1 = boolean, 2 = binary array, 3 = string)", ByteType, "1"),
      ByteEntityDescription(s"${index.toString}.5", "Value 1 bytes", "Value 1 type", "depends on value type", "\n...")
    )
  }

  def deserialize(buf: Array[Byte], offset: Int): Option[(List[DataEntry[_]], Int)] = {

    Try {

      val entryCount = Shorts.fromByteArray(buf.slice(offset, offset + 2))

      if (entryCount > 0) {
        val parsed = List.iterate(DataEntry.parse(buf, offset + 2), entryCount) { case (_, p) => DataEntry.parse(buf, p) }
        parsed.map(_._1) -> parsed.last._2
      } else
        List.empty -> (offset + 2)

    }.toOption
  }
}

case class FunctionCallBytes(index: Int, name: String) extends ByteEntity[Terms.FUNCTION_CALL] {

  def generateDoc(): Seq[ByteEntityDescription] = {
    Seq(ByteEntityDescription(index.toString, name, "EXPR", "F"))
  }

  def deserialize(buf: Array[Byte], offset: Int): Option[(Terms.FUNCTION_CALL, Int)] = {
    Try {
      val (expr, remaining) = Serde.deserialize(buf, all = false).explicitGet()
      expr.asInstanceOf[FUNCTION_CALL] -> (buf.length - remaining)
    }.toOption
  }
}

case class Composition[T1, T2](e1: ByteEntity[T1], e2: ByteEntity[T2]) extends ByteEntity[(T1, T2)] {

  val index = 0

  def generateDoc(): Seq[ByteEntityDescription] = e1.generateDoc() ++ e2.generateDoc()

  def deserialize(buf: Array[Byte], offset: Int): Option[((T1, T2), Int)] =
    for {
      (value1, offset1) <- e1.deserialize(buf, offset)
      (value2, offset2) <- e2.deserialize(buf, offset1)
    } yield ((value1, value2), offset2)
}
