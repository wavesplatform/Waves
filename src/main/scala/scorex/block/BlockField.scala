package scorex.block

import com.google.common.primitives.{Bytes, Ints, Longs}
import play.api.libs.json.{JsObject, Json}
import scorex.account.PublicKeyAccount
import scorex.crypto.encode.Base58
import scorex.serialization.{JsonSerializable, BytesSerializable}
import scorex.transaction.Transaction

/**
  * An abstraction of a part of a block, wrapping some data. The wrapper interface
  * provides binary & json serializations.
  *
  * @tparam T - type of a value wrapped into a blockfield
  */
abstract class BlockField[T] extends BytesSerializable with JsonSerializable {
  val name: String
  val value: T

}

case class ByteBlockField(override val name: String, override val value: Byte) extends BlockField[Byte] {

  override lazy val json: JsObject = Json.obj(name -> value.toInt)
  override lazy val bytes: Array[Byte] = Array(value)
}

case class IntBlockField(override val name: String, override val value: Int) extends BlockField[Int] {

  override lazy val json: JsObject = Json.obj(name -> value)
  override lazy val bytes: Array[Byte] = Bytes.ensureCapacity(Ints.toByteArray(value), 4, 0)
}

case class LongBlockField(override val name: String, override val value: Long) extends BlockField[Long] {

  override lazy val json: JsObject = Json.obj(name -> value)
  override lazy val bytes: Array[Byte] = Bytes.ensureCapacity(Longs.toByteArray(value), 8, 0)
}

case class BlockIdField(override val name: String, override val value: Block.BlockId)
  extends BlockField[Block.BlockId] {

  override lazy val json: JsObject = Json.obj(name -> Base58.encode(value))
  override lazy val bytes: Array[Byte] = value
}

case class TransactionBlockField(override val name: String, override val value: Transaction)
  extends BlockField[Transaction] {

  override lazy val json: JsObject = value.json
  override lazy val bytes: Array[Byte] = value.bytes
}

case class SignerData(generator: PublicKeyAccount, signature: Array[Byte])

case class SignerDataBlockField(override val name: String, override val value: SignerData)
  extends BlockField[SignerData] {

  override lazy val json: JsObject = Json.obj("generator" -> value.generator.toString,
    "signature" -> Base58.encode(value.signature))

  override lazy val bytes: Array[Byte] = value.generator.publicKey ++ value.signature
}
