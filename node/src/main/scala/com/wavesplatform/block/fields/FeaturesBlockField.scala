package com.wavesplatform.block.fields

import java.nio.ByteBuffer

import play.api.libs.json._
import com.wavesplatform.block.BlockField

case class FeaturesBlockField(version: Byte, override val value: Set[Short]) extends BlockField[Set[Short]] {
  override val name = "features"

  protected override def j: JsObject = version match {
    case v if v < 3 => JsObject.empty
    case _          => Json.obj(name -> JsArray(value.map(id => JsNumber(id.toInt)).toSeq))
  }

  protected override def b = version match {
    case v if v < 3 => Array.empty
    case _ =>
      val bb = ByteBuffer.allocate(Integer.BYTES + value.size * java.lang.Short.BYTES)
      bb.putInt(value.size).asShortBuffer().put(value.toArray)
      bb.array
  }
}
