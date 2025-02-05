package com.wavesplatform.ride

import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.lang.API
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3

object ScriptUtil {
  def from(src: String, libraries: Map[String, String] = Map.empty): Script =
    API
      .compile(
        input = src,
        estimator = ScriptEstimatorV3.latest,
        libraries = libraries
      )
      .flatMap(x => Script.fromBase64String(Base64.encode(x.bytes)))
      .explicitGet()
}
