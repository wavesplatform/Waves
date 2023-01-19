package com.wavesplatform.utils
import com.wavesplatform.lang.API
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3

import java.nio.file.Paths

object ExternalCompiler extends App {
  private val estimator = ScriptEstimatorV3(true, false)
  println(Paths.get(""))
  println(Paths.get("").toAbsolutePath.toString)
  println(args.head)
  println(API.compile(args.head, estimator))
}
