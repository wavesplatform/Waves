package com.wavesplatform.serialization

import io.swagger.annotations.ApiModelProperty
import monix.eval.Coeval

trait BytesSerializable {
  @ApiModelProperty(hidden = true)
  val bytes: Coeval[Array[Byte]]
}
