package com.wavesplatform.block

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.serialization.{BytesSerializable, JsonSerializable}
import com.wavesplatform.transaction.Transaction
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

abstract class BlockField[T] extends BytesSerializable with JsonSerializable {
  val name: String
  val value: T

  protected def j: JsObject

  protected def b: Array[Byte]

  val json: Coeval[JsObject]     = Coeval.evalOnce(j)
  val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(b)
}

case class ByteBlockField(override val name: String, override val value: Byte) extends BlockField[Byte] {
  protected override def j: JsObject = Json.obj(name -> value.toInt)

  protected override def b = Array(value)
}

case class LongBlockField(override val name: String, override val value: Long) extends BlockField[Long] {
  protected override def j: JsObject = Json.obj(name -> value)

  protected override def b: Array[Byte] = Bytes.ensureCapacity(Longs.toByteArray(value), 8, 0)
}

case class GeneratorBlockField(override val value: PublicKey) extends BlockField[PublicKey] {
  override val name: String = "generator"

  override protected def j: JsObject = Json.obj(name -> value.stringRepr, "generatorPublicKey" -> value.toString)

  override protected def b: Array[Byte] = value.arr
}

case class BlockIdField(override val name: String, override val value: Array[Byte]) extends BlockField[Array[Byte]] {
  protected override def j: JsObject = Json.obj(name -> Base58.encode(value))

  protected override def b: Array[Byte] = value
}

case class ByteStrBlockField(override val name: String, override val value: ByteStr) extends BlockField[ByteStr] {
  override protected def j: JsObject = Json.obj(name -> value.toString)

  override protected def b: Array[Byte] = value.arr
}

case class TransactionBlockField(override val name: String, override val value: Transaction) extends BlockField[Transaction] {
  protected override def j: JsObject = value.json()

  protected override def b: Array[Byte] = value.bytes()
}

case class SignerData(generator: PublicKey, signature: ByteStr)

case class SignerDataBlockField(override val name: String, override val value: SignerData) extends BlockField[SignerData] {
  protected override def j: JsObject =
    Json.obj("generator" -> value.generator.stringRepr, "generatorPublicKey" -> value.generator.toString, "signature" -> value.signature.toString)

  protected override def b: Array[Byte] = value.generator.arr ++ value.signature.arr
}
