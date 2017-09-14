package scorex.block.fields

import java.nio.ByteBuffer

import play.api.libs.json.JsObject
import scorex.block.BlockField

case class FeaturesBlockField(version: Byte, override val value: Set[Short]) extends BlockField[Set[Short]] {
  override val name = "features"

  override def json: JsObject = JsObject.empty

  override def bytes: Array[Byte] = version match {
    case v if v < 3 => Array.empty
    case _ =>
      val bb = ByteBuffer.allocate(Integer.BYTES + value.size * java.lang.Short.BYTES)
      bb.putInt(value.size).asShortBuffer().put(value.toArray)
      bb.array
  }
}

