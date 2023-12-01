package com.wavesplatform.ride.runner

import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.lang.API
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3

object TestScript {
  def scriptFrom(src: String): Script =
    API
      .compile(input = src, ScriptEstimatorV3.latest)
      .flatMap(x => Script.fromBase64String(Base64.encode(x.bytes)))
      .explicitGet()
}
