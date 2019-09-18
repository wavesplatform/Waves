package com.wavesplatform.block.fields

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.block.{Block, BlockField}
import play.api.libs.json.{JsNumber, JsObject, Json}

case class RewardBlockField(version: Byte, override val value: Long) extends BlockField[Long] {
  override val name: String = "desiredReward"

  override protected def j: JsObject =
    if (version < Block.RewardBlockVersion) JsObject.empty
    else Json.obj(name -> JsNumber(value))

  override protected def b: Array[Byte] =
    if (version < Block.RewardBlockVersion) Array.empty
    else Bytes.ensureCapacity(Longs.toByteArray(value), 8, 0)
}
