package com.wavesplatform.lang.v1.repl

import com.wavesplatform.lang.v1.repl.node.http.response.model.NodeResponse

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js

object Global {
  def requestNode(url: String): Future[NodeResponse] =
    impl.Global
      .httpGet(js.Dynamic.literal(url = url))
      .toFuture
      .map(r => NodeResponse(r.status.asInstanceOf[Int], r.body.asInstanceOf[String]))(ExecutionContext.global)
}
