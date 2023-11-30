package com.wavesplatform.ride.runner

import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.lang.API
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3

object TestScript {
  val estimatorV3 = ScriptEstimatorV3(fixOverflow = true, overhead = false)
  def scriptFrom(src: String): Script =
    API
      .compile(input = src, estimatorV3)
      .flatMap(x => Script.fromBase64String(Base64.encode(x.bytes)))
      .explicitGet()
}
