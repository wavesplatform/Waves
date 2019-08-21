package com.wavesplatform.block.fields

import com.wavesplatform.block.{Block, BlockField}
import play.api.libs.json.{JsNumber, JsObject, Json}

case class RewardBlockField(version: Byte, override val value: Byte) extends BlockField[Byte] {
  override val name: String = "reward"

  override protected def j: JsObject =
    if (version < Block.RewardBlockVersion) JsObject.empty
    else Json.obj(name -> JsNumber(value.toInt))

  override protected def b: Array[Byte] =
    if (version < Block.RewardBlockVersion) Array.empty
    else Array(value)
}
