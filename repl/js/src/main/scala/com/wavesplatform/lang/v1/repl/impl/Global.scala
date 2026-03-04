package com.wavesplatform.lang.v1.repl.impl

import scala.scalajs.js
import scala.scalajs.js.Promise

object Global {
  def httpGet(params: js.Dynamic): Promise[js.Dynamic] =
    com.wavesplatform.lang.impl.Global.httpGet(params)
}
