package com.wavesplatform.lang.v1.repl

import com.wavesplatform.lang.SttpClient
import com.wavesplatform.lang.v1.repl.node.http.response.model.NodeResponse

import scala.concurrent.Future

object Global {
  private val client = new SttpClient()

  def requestNode(url: String): Future[NodeResponse] = client.requestNode(url)
}
