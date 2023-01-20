package com.wavesplatform.utils
import com.wavesplatform.lang.API
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*

object FileCompiler extends App {
  private val estimator = ScriptEstimatorV3(true, false)
  args
    .foreach { path =>
      val script = Files.readAllLines(Paths.get(path)).asScala.reduce(_ + "\n" + _)
      API
        .compile(script, estimator)
        .fold(
          error => throw new RuntimeException(s"$error"),
          _ => println(s"successfully compiled $path")
        )
    }
}
