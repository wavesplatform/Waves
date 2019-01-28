package com.wavesplatform.transaction.description

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.account.{Address, AddressOrAlias, Alias, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto.{KeyLength, SignatureLength}
import com.wavesplatform.transaction.{AssetId, Proofs, ValidationError}
import com.wavesplatform.transaction.ValidationError.Validation
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.{ParsedTransfer, Transfer}

import scala.util.Try

case class ByteEntityDescription(index: String, name: String, tpe: String, length: String, additionalInfo: String = "")

sealed trait ByteEntity[T] { self =>

  //def generateDoc(): String
  def generateDoc(): Seq[ByteEntityDescription]

  def deserialize(buf: Array[Byte], offset: Int): Option[(T, Int)]

  def deserializeFromByteArray(buf: Array[Byte]): Option[T] = deserialize(buf, 0) map { case (value, _) ⇒ value }

  def ~[U](other: ByteEntity[U]): ByteEntity[(T, U)] = Composition(this, other)

  def |(other: ByteEntity[T]): ByteEntity[T] = new ByteEntity[T] {

    //def generateDoc(): String = s"${self.generateDoc()} | ${other.generateDoc()}"
    def generateDoc(): Seq[ByteEntityDescription] = self.generateDoc() ++ other.generateDoc()

    def deserialize(buf: Array[Byte], offset: Int): Option[(T, Int)] =
      self.deserialize(buf, offset).orElse(other.deserialize(buf, offset))
  }

  def map[U](f: T => U): ByteEntity[U] = new ByteEntity[U] {

    def generateDoc(): Seq[ByteEntityDescription] = self.generateDoc()

    def deserialize(buf: Array[Byte], offset: Int): Option[(U, Int)] = self.deserialize(buf, offset).map { case (t, o) => f(t) -> o }
  }

  def getDoc(): String = {

    val (names, types, lengths) = generateDoc().linesIterator.toList
      .withFilter(_ != "...")
      .map { line =>
        line.split(";") match {
          case Array(name, tpe, length) => (name, tpe, length)
        }
      }
      .unzip3

    val namesMaxLength = names.map(_.length).max
    val typesMaxLength = types.map(_.length).max

    names
      .map(_.padTo(namesMaxLength, " ").mkString)
      .zip(types.map(_.padTo(typesMaxLength, " ").mkString))
      .map { case (name, tpe) => s"$name$tpe" }
      .zip(lengths)
      .map { case (nameAndType, length) => s"$nameAndType$length" }
      .zipWithIndex
      .map { case (line, index) => s"${((index + 1).toString + ".").padTo(3, " ").mkString} $line" }
      .mkString("\n")
  }
}

case class Constant(value: Byte, name: String) extends ByteEntity[Byte] {

  def generateDoc(): String = s"Field name: $name; Type: Byte (constant, value = $value); Length: 1"

  def deserialize(buf: Array[Byte], offset: Int): Option[(Byte, Int)] = Some(value -> (offset + 1))
}

case class OneByte(name: String) extends ByteEntity[Byte] {

  def generateDoc(): String = s"Field name: $name; Type: Byte; Length: 1"

  def deserialize(buf: Array[Byte], offset: Int): Option[(Byte, Int)] = {
    Try { buf(offset) -> (offset + 1) }.toOption
  }
}

case class LongBytes(name: String) extends ByteEntity[Long] {

  def generateDoc(): String = s"Field name: $name; Type: Long; Length: 8"

  def deserialize(buf: Array[Byte], offset: Int): Option[(Long, Int)] = {
    Try { Longs.fromByteArray(buf.slice(offset, offset + 8)) -> (offset + 8) }.toOption
  }
}

case class BooleanByte(name: String) extends ByteEntity[Boolean] {

  def generateDoc(): String = s"Field name: $name; Type: Boolean; Length: 1"

  def deserialize(buf: Array[Byte], offset: Int): Option[(Boolean, Int)] = {
    Try { (buf(offset) == 1) -> (offset + 1) }.toOption
  }
}

case class BytesArrayDefinedLength(name: String, length: Int) extends ByteEntity[Array[Byte]] {

  def generateDoc(): String = s"Field name: $name; Type: Array[Byte]; Length: $length"

  def deserialize(buf: Array[Byte], offset: Int): Option[(Array[Byte], Int)] = {
    Try { buf.slice(offset, offset + length) -> (offset + length) }.toOption
  }
}

case class BytesArrayUndefinedLength(name: String) extends ByteEntity[Array[Byte]] {

  def generateDoc(): String = {
    s"Field name: $name length (N); Type: Short; Length: 2\n" +
      s"Field name: $name; Type: Array[Byte]; Length: N"
  }

  def deserialize(buf: Array[Byte], offset: Int): Option[(Array[Byte], Int)] = {
    Try {
      val length                 = Shorts.fromByteArray(buf.slice(offset, offset + 2))
      val (arrayStart, arrayEnd) = (offset + 2, offset + 2 + length)
      buf.slice(arrayStart, arrayEnd) -> arrayEnd
    }.toOption
  }

}

case class PublicKeyAccountBytes(name: String) extends ByteEntity[PublicKeyAccount] {

  def generateDoc(): String = s"Field name: $name; Type: PublicKeyAccount (Array[Byte]); Length: $KeyLength"

  def deserialize(buf: Array[Byte], offset: Int): Option[(PublicKeyAccount, Int)] = {
    Try { PublicKeyAccount(buf.slice(offset, offset + KeyLength)) -> (offset + KeyLength) }.toOption
  }
}

case class SignatureBytes(name: String) extends ByteEntity[ByteStr] {

  def generateDoc(): String = s"Field name: $name; Type: ByteStr (Array[Byte]); Length: $SignatureLength"

  def deserialize(buf: Array[Byte], offset: Int): Option[(ByteStr, Int)] = {
    Try { ByteStr(buf.slice(offset, offset + SignatureLength)) -> (offset + SignatureLength) }.toOption
  }
}

case class OptionAssetIdBytes(name: String) extends ByteEntity[Option[AssetId]] {

  import com.wavesplatform.transaction.AssetIdLength

  def generateDoc(): String = {
    s"Field name: $name - flag (0 - Waves, 1 - asset); Type: Byte; Length: 1\n" +
      s"Field name: $name; Type: Array[Byte]; Length: 0/$AssetIdLength"
  }

  def deserialize(buf: Array[Byte], offset: Int): Option[(Option[AssetId], Int)] = {
    Try {
      if (buf(offset) == (1: Byte)) Some(ByteStr(buf.slice(offset + 1, offset + 1 + AssetIdLength))) -> (offset + 1 + AssetIdLength)
      else (None, offset + 1)
    }.toOption
  }
}

case class AddressOrAliasBytes(name: String) extends ByteEntity[AddressOrAlias] {

  def generateDoc(): String = s"Field name: $name; Type: Address or Alias; Length: depends on first byte (see Address/Alias data structures)"

  def deserialize(buf: Array[Byte], offset: Int): Option[(AddressOrAlias, Int)] = {
    AddressOrAlias.fromBytes(buf, offset).toOption
  }
}

case object ProofsBytes extends ByteEntity[Proofs] {

  def generateDoc(): String = {
    s"Field name: Proofs version (0x01); Type: Byte; Length: 1\n" +
      s"Field name: Proofs count; Type: Short; Length: 2\n" +
      s"Field name: Proof 1 length (N); Type: Short; Length: 2\n" +
      s"Field name: Proof 1; Type: ByteStr (Array[Byte]); Length: N\n" +
      s"Field name: Proof 2 length (M); Type: Short; Length: 2\n" +
      s"Field name: Proof 2; Type: ByteStr (Array[Byte]); Length: M\n" +
      s"..."
  }

  def deserialize(buf: Array[Byte], offset: Int): Option[(Proofs, Int)] = {
    Proofs.fromBytes(buf.drop(offset)).toOption.map { p =>
      p -> (offset + p.bytes.value.length)
    }
  }
}

case object TransfersBytes extends ByteEntity[List[ParsedTransfer]] {

  import cats.implicits._

  private def readTransfer(buf: Array[Byte], offset: Int): (Validation[ParsedTransfer], Int) = {
    AddressOrAlias.fromBytes(buf, offset) match {
      case Right((addressOrAlias, ofs)) =>
        val amount = Longs.fromByteArray(buf.slice(ofs, ofs + 8))
        Right[ValidationError, ParsedTransfer](ParsedTransfer(addressOrAlias, amount)) -> (ofs + 8)
      case Left(validationError) => Left(validationError) -> offset
    }
  }

  def generateDoc(): String = {
    s"Field name: Number of transfers; Type: Short; Length: 2\n" +
      s"Field name: Address or alias for transfer 1; Type: Address or Alias; Length: depends on first byte (see Address/Alias data structures)\n" +
      s"Field name: Amount for transfer 1; Type: Long; Length: 8\n" +
      s"Field name: Address or alias for transfer 2; Type: Address or Alias; Length: depends on first byte (see Address/Alias data structures)\n" +
      s"Field name: Amount for transfer 2; Type: Long; Length: 8\n" +
      s"..."

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

//case class OptionBytes(name: String, length: Int) extends ByteEntity[Array[Byte]] {
//
//  val somePart = (Constant(1, "of") ~ Bytes("ov", length)).map(_._2)
//  val nonePart = Bytes("None", 1)
//
//  def generateDoc(): String = (somePart | nonePart).generateDoc()
//
//  def deserialize(buf: Array[Byte], offset: Int): Option[(Array[Byte], Int)] = {
//
//    println(somePart.deserialize(buf, offset).map { case (arr, offset) => arr.mkString(", ") -> offset })
//    println(nonePart.deserialize(buf, offset).map { case (arr, offset) => arr.mkString(", ") -> offset })
//
//    (somePart | nonePart).deserialize(buf, offset)
//  }
//}

case class Composition[T1, T2](e1: ByteEntity[T1], e2: ByteEntity[T2]) extends ByteEntity[(T1, T2)] {

  def generateDoc(): String = s"${e1.generateDoc()}\n${e2.generateDoc()}"

  def deserialize(buf: Array[Byte], offset: Int): Option[((T1, T2), Int)] =
    for {
      (v1, o2)   <- e1.deserialize(buf, offset)
      (v2, rest) <- e2.deserialize(buf, o2)
    } yield ((v1, v2), rest)
}

object ByteEntity extends App {

  import com.wavesplatform.common.utils.EitherExt2

//  case class TestTransaction(typeId: Byte, signature: Array[Byte], chainId: Byte, assetID: Option[AssetId], testByte: Byte) {
//    override def toString: String =
//      s"TestTransaction(typeId = $typeId, signature = ${signature.mkString(", ")}, chainId = $chainId, assetId = ${assetID.fold("Waves")(
//        _.arr.mkString(", "))})"
//  }
//
//  val byteArray = Array[Byte](0, 1, 0, 5, 15, 20, 25, 30, 35, 77, 0, 15)
//
//  val byteDesc =
//    (Constant(0, "mark") ~ OneByte("typeId") ~ BytesArrayUndefinedLength("signature") ~ OneByte("chainId") ~ OptionAssetIdBytes("assetId") ~ OneByte(
//      "test"))
//      .map { case (((((_, ti), s), ci), oai), tb) => TestTransaction(ti, s, ci, oai, tb) }
//
//  val parsedTransferByteDesc = (AddressOrAliasBytes("AddressOrAlias object for transfer 1") ~ LongBytes("Amount for transfer 1")).map {
//    case (aoa, a) => ParsedTransfer(aoa, a)
//  }
//
//  println(byteDesc.deserializeFromByteArray(byteArray, 0))
//  println(byteDesc.getDoc())

  case class TestMassTransaction(version: Byte,
                                 assetId: Option[AssetId],
                                 sender: PublicKeyAccount,
                                 transfers: List[ParsedTransfer],
                                 timestamp: Long,
                                 fee: Long,
                                 attachments: Array[Byte],
                                 proofs: Proofs) {

    val typeId = 11: Byte

    val assetIdBytes = assetId.map(a => (1: Byte) +: a.arr).getOrElse(Array(0: Byte))

    val transferBytes = transfers
      .map { case ParsedTransfer(recipient, amount) => recipient.bytes.arr ++ Longs.toByteArray(amount) }
      .fold(Array())(_ ++ _)

    val bytes =
      Bytes.concat(
        Array(typeId, version),
        sender.publicKey,
        assetIdBytes,
        Shorts.toByteArray(transfers.size.toShort),
        transferBytes,
        Longs.toByteArray(timestamp),
        Longs.toByteArray(fee),
        Shorts.toByteArray(attachments.length.toShort),
        attachments,
        proofs.bytes.value
      )

    override def toString: String =
      "TestMassTransaction(" +
        s"version = $version, " +
        s"assetId = ${assetId.fold("Waves")(_.arr.mkString(", "))}, " +
        s"sender = $sender, " +
        s"transfers = ${transfers.mkString(", ")}, " +
        s"timestamp = $timestamp, " +
        s"fee = $fee, " +
        s"attachments = $attachments, " +
        s"proofs = ${proofs.proofs.mkString(", ")}"
    s")"
  }

  val transfers = MassTransferTransaction
    .parseTransfersList(
      List(Transfer("3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh", 100000000L), Transfer("3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh", 200000000L)))
    .right
    .get

  val tmttBytes =
    TestMassTransaction(
      1,
      None,
      PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
      transfers,
      1518091313964L,
      200000,
      Base58.decode("59QuUcqP6p").get,
      Proofs(Seq(ByteStr.decodeBase58("FXMNu3ecy5zBjn9b69VtpuYRwxjCbxdkZ3xZpLzB8ZeFDvcgTkmEDrD29wtGYRPtyLS3LPYrL2d5UM6TpFBMUGQ").get))
    ).bytes

  val tmttByteDesc =
    (
      OneByte("Transaction type") ~
        OneByte("Version") ~
        PublicKeyAccountBytes("Sender's public key") ~
        OptionAssetIdBytes("Asset") ~
        TransfersBytes ~
        LongBytes("Timestamp") ~
        LongBytes("Fee") ~
        BytesArrayUndefinedLength("Attachments") ~
        ProofsBytes
    ).map {
      case ((((((((_, version), sender), assetId), transfer), timestamp), fee), attachments), proofs) =>
        TestMassTransaction(
          version = version,
          assetId = assetId,
          sender = sender,
          transfers = transfer,
          timestamp = timestamp,
          fee = fee,
          attachments = attachments,
          proofs = proofs
        )
    }

  println(tmttByteDesc.deserializeFromByteArray(tmttBytes))
  println(tmttByteDesc.getDoc())

}
