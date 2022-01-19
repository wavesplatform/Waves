package com.wavesplatform.lang.v1.repl

import scala.concurrent.ExecutionContext

object JsCompat {
  implicit def executionContext: ExecutionContext = org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits.global
}
