package com.wavesplatform.transaction.description

import cats.{Functor, Semigroupal}
import com.google.common.primitives.{Ints, Longs, Shorts}
import com.wavesplatform.account.{Address, AddressOrAlias, Alias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto.{KeyLength, SignatureLength}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.{Script, ScriptReader}
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.{ContractLimits, Serde}
import com.wavesplatform.serialization.Deser
import com.wavesplatform.state.DataEntry
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.Validation
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer

import scala.util.{Failure, Success, Try}

/**
  * Represents description of the byte entity
  * Field `additionalInfo` can be used for specifying of the repeating byte entities
  */
case class ByteEntityDescription(index: Int, name: String, tpe: String, length: String, subIndex: Int = 0, additionalInfo: String = "")

/**
  * Describes byte representation of the different types. Composition of Byte Entities can be used for deserialization
  * and generation of the documentation of the complex data structures, such as transactions, messages, orders, etc
  */
sealed trait ByteEntity[T] { self =>

  private[description] val ByteType           = "Byte"
  private[description] val BooleanType        = "Boolean"
  private[description] val IntType            = "Int"
  private[description] val LongType           = "Long"
  private[description] val ByteArrayType      = "Array[Byte]"
  private[description] val ByteStrType        = s"ByteStr ($ByteArrayType)"
  private[description] val AddressType        = "Address"
  private[description] val AliasType          = "Alias"
  private[description] val AddressOrAliasType = "Address or Alias"
  private[description] val ProofsType         = "Proofs"
  private[description] val UnimportantType    = ""

  /** Index of the byte entity. In case of composition of byte entities returns index of the last one */
  val index: Int

  private[description] def generateDoc: Seq[ByteEntityDescription]

  private[description] def deserialize(buf: Array[Byte], offset: Int): Try[(T, Int)]

  def deserializeFromByteArray(buf: Array[Byte]): Try[T] = deserialize(buf, 0) map { case (value, _) => value }

  def map[U](f: T => U): ByteEntity[U] = new ByteEntity[U] {

    val index: Int = self.index

    def generateDoc: Seq[ByteEntityDescription] = self.generateDoc

    def deserialize(buf: Array[Byte], offset: Int): Try[(U, Int)] = self.deserialize(buf, offset).map { case (t, o) => f(t) -> o }
  }

  /** Generates documentation ready for pasting into .md files */
  def getStringDocForMD: String = {
    generateDoc
      .map {
        case ByteEntityDescription(idx, name, tpe, length, subIndex, additionalInfo) =>
          s"| $idx${Option(subIndex).filter(_ != 0).fold("")(si => s".$si")} | $name | $tpe | $length $additionalInfo\n"
            .replace("...", "| ... | ... | ... | ... |")
            .replace("(", "\\(")
            .replace(")", "\\)")
            .replace("*", "\\*")
      }
      .foldLeft("""| \# | Field name | Type | Length in Bytes |""" + "\n| --- | --- | --- | --- |\n")(_ + _)
  }
}

object ByteEntity {

  implicit val byteEntityFunctor: Functor[ByteEntity] = new Functor[ByteEntity] {
    def map[A, B](fa: ByteEntity[A])(f: A => B): ByteEntity[B] = fa map f
  }

  implicit val byteEntitySemigroupal: Semigroupal[ByteEntity] = new Semigroupal[ByteEntity] {
    def product[A, B](fa: ByteEntity[A], fb: ByteEntity[B]): ByteEntity[(A, B)] = Composition(fa, fb)
  }
}

case class ConstantByte(index: Int, value: Byte, name: String) extends ByteEntity[Byte] {

  def generateDoc: Seq[ByteEntityDescription] = Seq(ByteEntityDescription(index, name, s"$ByteType (constant, value = $value)", "1"))

  def deserialize(buf: Array[Byte], offset: Int): Try[(Byte, Int)] = {
    Try { value -> (offset + 1) }
  }
}

case class OneByte(index: Int, name: String) extends ByteEntity[Byte] {

  def generateDoc: Seq[ByteEntityDescription] = Seq(ByteEntityDescription(index, name, ByteType, "1"))

  def deserialize(buf: Array[Byte], offset: Int): Try[(Byte, Int)] = {
    Try { buf(offset) -> (offset + 1) }
  }
}

case class LongBytes(index: Int, name: String) extends ByteEntity[Long] {

  def generateDoc: Seq[ByteEntityDescription] = Seq(ByteEntityDescription(index, name, LongType, "8"))

  def deserialize(buf: Array[Byte], offset: Int): Try[(Long, Int)] = {
    Try { Longs.fromByteArray(buf.slice(offset, offset + 8)) -> (offset + 8) }
  }
}

case class IntBytes(index: Int, name: String) extends ByteEntity[Int] {

  def generateDoc: Seq[ByteEntityDescription] = Seq(ByteEntityDescription(index, name, IntType, "4"))

  def deserialize(buf: Array[Byte], offset: Int): Try[(Int, Int)] = {
    Try { Ints.fromByteArray(buf.slice(offset, offset + 4)) -> (offset + 4) }
  }
}

case class BooleanByte(index: Int, name: String) extends ByteEntity[Boolean] {

  def generateDoc: Seq[ByteEntityDescription] = Seq(ByteEntityDescription(index, name, BooleanType, "1"))

  def deserialize(buf: Array[Byte], offset: Int): Try[(Boolean, Int)] = {
    Try { (buf(offset) == 1) -> (offset + 1) }
  }
}

case class BytesArrayDefinedLength(index: Int, name: String, length: Int) extends ByteEntity[Array[Byte]] {

  def generateDoc: Seq[ByteEntityDescription] = Seq(ByteEntityDescription(index, name, ByteArrayType, length.toString))

  def deserialize(buf: Array[Byte], offset: Int): Try[(Array[Byte], Int)] = {
    Try { buf.slice(offset, offset + length) -> (offset + length) }
  }
}

case class BytesArrayUndefinedLength(index: Int, name: String, maxLength: Int, minLength: Int = 0) extends ByteEntity[Array[Byte]] {

  def generateDoc: Seq[ByteEntityDescription] = {
    Seq(
      ByteEntityDescription(index, s"$name length (N)", UnimportantType, "2", subIndex = 1),
      ByteEntityDescription(index, name, ByteArrayType, s"${if (minLength == 0) "" else s"$minLength <= "}N <= $maxLength", subIndex = 2)
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

  def generateDoc: Seq[ByteEntityDescription] = {
    Seq(ByteEntityDescription(index, name, ByteStrType, length.toString))
  }

  def deserialize(buf: Array[Byte], offset: Int): Try[(ByteStr, Int)] = {
    Try { ByteStr(buf.slice(offset, offset + length)) -> (offset + length) }
  }
}

case class PublicKeyBytes(index: Int, name: String) extends ByteEntity[PublicKey] {

  def generateDoc: Seq[ByteEntityDescription] =
    Seq(ByteEntityDescription(index, name, s"PublicKey ($ByteArrayType)", KeyLength.toString))

  def deserialize(buf: Array[Byte], offset: Int): Try[(PublicKey, Int)] = {
    Try { PublicKey(buf.slice(offset, offset + KeyLength)) -> (offset + KeyLength) }
  }
}

case class SignatureBytes(index: Int, name: String) extends ByteEntity[ByteStr] {

  def generateDoc: Seq[ByteEntityDescription] = Seq(ByteEntityDescription(index, name, ByteStrType, SignatureLength.toString))

  def deserialize(buf: Array[Byte], offset: Int): Try[(ByteStr, Int)] = {
    Try { ByteStr(buf.slice(offset, offset + SignatureLength)) -> (offset + SignatureLength) }
  }
}

case class SponsorFeeOptionLongBytes(index: Int, name: String) extends ByteEntity[Option[Long]] {

  def generateDoc: Seq[ByteEntityDescription] = {
    Seq(ByteEntityDescription(index, name, LongType, "8"))
  }

  def deserialize(buf: Array[Byte], offset: Int): Try[(Option[Long], Int)] = {
    Try { Option(Longs.fromByteArray(buf.slice(offset, offset + 8))).filter(_ != 0) -> (offset + 8) }
  }
}

case class AddressBytes(index: Int, name: String) extends ByteEntity[Address] {

  def generateDoc: Seq[ByteEntityDescription] = {
    Seq(ByteEntityDescription(index, name, AddressType, s"${Address.AddressLength}"))
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

  def generateDoc: Seq[ByteEntityDescription] = {
    Seq(
      ByteEntityDescription(index, s"$name length (A)", UnimportantType, "2", subIndex = 1),
      ByteEntityDescription(index, s"$name", AliasType, s"${Alias.MinLength} <= A <= ${Alias.MaxLength}", subIndex = 2)
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

  def generateDoc: Seq[ByteEntityDescription] =
    Seq(ByteEntityDescription(index, name, AddressOrAliasType, "Depends on the first byte (1 - Address, 2 - Alias)"))

  def deserialize(buf: Array[Byte], offset: Int): Try[(AddressOrAlias, Int)] = {
    Try { AddressOrAlias.fromBytes(buf, offset).explicitGet() }
  }
}

case class ProofsBytes(index: Int, concise: Boolean = true) extends ByteEntity[Proofs] {

  def generateDoc: Seq[ByteEntityDescription] = {
    if (concise) Seq(ByteEntityDescription(index, s"Proofs", ProofsType, "See Proofs structure"))
    else
      Seq(
        ByteEntityDescription(index, s"Proofs version (${Proofs.Version})", UnimportantType, "1", subIndex = 1),
        ByteEntityDescription(index, "Proofs count", UnimportantType, "2", subIndex = 2),
        ByteEntityDescription(index, "Proof 1 length (P1)", UnimportantType, "2", subIndex = 3),
        ByteEntityDescription(index, "Proof 1", ByteStrType, s"P1 <= ${Proofs.MaxProofSize}", subIndex = 4),
        ByteEntityDescription(index, "Proof 2 length (P2)", UnimportantType, "2", subIndex = 5),
        ByteEntityDescription(index, "Proof 2 ", ByteStrType, s"P2 <= ${Proofs.MaxProofSize}", subIndex = 6, additionalInfo = "\n...")
      )
  }

  def deserialize(buf: Array[Byte], offset: Int): Try[(Proofs, Int)] = {
    Try { Proofs.fromBytes(buf.drop(offset)).map(p => p -> (offset + p.bytes.value.length)).explicitGet() }
  }
}

case class FunctionCallBytes(index: Int, name: String) extends ByteEntity[Terms.FUNCTION_CALL] {

  def generateDoc: Seq[ByteEntityDescription] = {
    Seq(ByteEntityDescription(index, name, "EXPR", "F"))
  }

  def deserialize(buf: Array[Byte], offset: Int): Try[(Terms.FUNCTION_CALL, Int)] = {
    Try {
      val (expr, remaining) = Serde.deserialize(buf.drop(offset), all = false).explicitGet()
      expr.asInstanceOf[FUNCTION_CALL] -> (buf.length - remaining)
    }
  }
}

case class AssetIdBytes(index: Int, name: String) extends ByteEntity[IssuedAsset] {

  def generateDoc: Seq[ByteEntityDescription] = {
    Seq(ByteEntityDescription(index, name, "AssetId (ByteStr = Array[Byte])", AssetIdLength.toString))
  }

  def deserialize(buf: Array[Byte], offset: Int): Try[(IssuedAsset, Int)] = {
    Try {
      val bytes = ByteStr(buf.slice(offset, offset + AssetIdLength))
      val off   = offset + AssetIdLength

      IssuedAsset(bytes) -> off
    }
  }
}

case class ScriptBytes(index: Int, name: String) extends ByteEntity[Script] {

  def generateDoc: Seq[ByteEntityDescription] = {
    Seq(
      ByteEntityDescription(index, s"$name length (S)", UnimportantType, "2", subIndex = 1),
      ByteEntityDescription(index, name, "Script", s"S <= ${ContractLimits.MaxContractSizeInBytes}", subIndex = 2)
    )
  }

  def deserialize(buf: Array[Byte], offset: Int): Try[(Script, Int)] = {
    Try {
      val scriptLength = Shorts.fromByteArray(buf.slice(offset, offset + 2))
      ScriptReader.fromBytes(buf.slice(offset + 2, offset + 2 + scriptLength)).explicitGet() -> (offset + 2 + scriptLength)
    }
  }
}

case class PaymentBytes(index: Int, name: String) extends ByteEntity[Payment] {

  def generateDoc: Seq[ByteEntityDescription] = {
    Seq(
      ByteEntityDescription(index, s"$name length (P)", UnimportantType, "2", subIndex = 1),
      ByteEntityDescription(index, name, s"$name (Long, Option[AssetId])", s"P <= ${8 + AssetIdLength}", subIndex = 2)
    )
  }

  def deserialize(buf: Array[Byte], offset: Int): Try[(Payment, Int)] = {
    Try {

      val paymentLength     = Shorts.fromByteArray(buf.slice(offset, offset + 2))
      val arr               = buf.slice(offset + 2, offset + 2 + paymentLength)
      val amt: Long         = Longs.fromByteArray(arr.take(8))
      val (maybeAssetId, _) = Deser.parseOption(arr, 8, 32)(ByteStr.apply)
      val asset             = maybeAssetId.fold[Asset](Waves)(IssuedAsset)

      Payment(amt, asset) -> (offset + 2 + paymentLength)
    }
  }
}

/**
  *  Represents byte description of Option[U]
  *
  *  @param nestedByteEntity         describes byte entity of type U
  *  @param firstByteInterpretation  how to interpret first byte
  */
class OptionBytes[U](val index: Int, name: String, nestedByteEntity: ByteEntity[U], firstByteInterpretation: String = "existence flag (1/0)")
    extends ByteEntity[Option[U]] {

  def generateDoc: Seq[ByteEntityDescription] = {
    ByteEntityDescription(index, s"$name $firstByteInterpretation", UnimportantType, "1", subIndex = 1) +:
      nestedByteEntity.generateDoc.map { desc =>
        desc.copy(
          length = desc.length + s" or 0 (depends on the byte in $index.1)",
          subIndex = if (desc.subIndex != 0) desc.subIndex + 1 else desc.subIndex + 2
        )
      }
  }

  def deserialize(buf: Array[Byte], offset: Int): Try[(Option[U], Int)] = {
    if (buf(offset) == 1) nestedByteEntity.deserialize(buf, offset + 1).map { case (value, offst) => Some(value) -> offst } else
      Try { None -> (offset + 1) }
  }
}

object OptionBytes {
  def apply[U](
      index: Int,
      name: String,
      nestedByteEntity: ByteEntity[U],
      firstByteInterpretation: String = "existence flag (1/0)"
  ): ByteEntity[Option[U]] =
    new OptionBytes(index, name, nestedByteEntity, firstByteInterpretation)
}

class SeqBytes[U](val index: Int, name: String, nestedByteEntity: ByteEntity[U]) extends ByteEntity[Seq[U]] {

  def generateDoc: Seq[ByteEntityDescription] = {
    val seq =
      ByteEntityDescription(index, s"$name size", UnimportantType, "2", subIndex = 1) +:
        nestedByteEntity.generateDoc.map { desc =>
          desc.copy(
            length = desc.length + s" or 0 (depends on the short in $index.1)",
            subIndex = if (desc.subIndex != 0) desc.subIndex + 1 else desc.subIndex + 2
          )
        }

    seq.init :+ seq.last.copy(additionalInfo = "\n...")
  }

  def deserialize(buf: Array[Byte], offset: Int): Try[(Seq[U], Int)] = {
    Try {
      val entryCount = Shorts.fromByteArray(buf.slice(offset, offset + 2))

      if (entryCount < 0 || entryCount > buf.size - offset - 2) {
        throw new Exception(s"Brocken array size ($entryCount entries while ${buf.size - offset - 2} bytes avaliable)")
      } else if (entryCount > 0) {
        val parsed = List.iterate(nestedByteEntity.deserialize(buf, offset + 2).get, entryCount) {
          case (_, p) => nestedByteEntity.deserialize(buf, p).get
        }
        parsed.map(_._1) -> parsed.last._2
      } else
        List.empty -> (offset + 2)
    }
  }
}

object SeqBytes {
  def apply[U](index: Int, name: String, nestedByteEntity: ByteEntity[U]): ByteEntity[Seq[U]] =
    new SeqBytes[U](index, name, nestedByteEntity)
}
case class Composition[T1, T2](e1: ByteEntity[T1], e2: ByteEntity[T2]) extends ByteEntity[(T1, T2)] {

  val index: Int = e2.index // use last index in composition

  def generateDoc: Seq[ByteEntityDescription] = e1.generateDoc ++ e2.generateDoc

  def deserialize(buf: Array[Byte], offset: Int): Try[((T1, T2), Int)] =
    for {
      (value1, offset1) <- e1.deserialize(buf, offset)
      (value2, offset2) <- e2.deserialize(buf, offset1)
    } yield ((value1, value2), offset2)
}
