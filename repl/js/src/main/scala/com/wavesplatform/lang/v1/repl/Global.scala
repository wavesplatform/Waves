package com.wavesplatform.lang.v1.repl

import scala.concurrent.Future
import scala.scalajs.js

import com.wavesplatform.lang.v1.repl.node.http.response.model.NodeResponse
import com.wavesplatform.lang.v1.repl.node.http.WebEnvironment.executionContext

object Global {
  def requestNode(url: String): Future[NodeResponse] =
    impl.Global
      .httpGet(js.Dynamic.literal(url = url))
      .toFuture
      .map(r => NodeResponse(r.status.asInstanceOf[Int], r.body.asInstanceOf[String]))(executionContext)
}
