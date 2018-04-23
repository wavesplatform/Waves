package scorex.block

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.state.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account.PublicKeyAccount
import scorex.crypto.encode.Base58
import scorex.serialization.{BytesSerializable, JsonSerializable}
import scorex.transaction.Transaction

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

  protected override def b = Bytes.ensureCapacity(Longs.toByteArray(value), 8, 0)
}

case class BlockIdField(override val name: String, override val value: Array[Byte]) extends BlockField[Array[Byte]] {
  protected override def j: JsObject = Json.obj(name -> Base58.encode(value))

  protected override def b = value
}

case class TransactionBlockField(override val name: String, override val value: Transaction) extends BlockField[Transaction] {
  protected override def j: JsObject = value.json()

  protected override def b = value.bytes()
}

case class SignerData(generator: PublicKeyAccount, signature: ByteStr)

case class SignerDataBlockField(override val name: String, override val value: SignerData) extends BlockField[SignerData] {
  protected override def j = Json.obj("generator" -> value.generator.toString, "signature" -> value.signature.base58)

  protected override def b = value.generator.publicKey ++ value.signature.arr
}
