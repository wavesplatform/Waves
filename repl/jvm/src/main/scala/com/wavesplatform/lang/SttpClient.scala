package com.wavesplatform.lang

import com.wavesplatform.lang.v1.repl.node.http.response.model.NodeResponse
import sttp.client3.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SttpClient {
  private val backend: SttpBackend[Identity, Any] = HttpURLConnectionBackend()
  private val schemaRegex                         = "^\\w+://.+".r

  def requestNode(url: String): Future[NodeResponse] = {
    val urlPrefix =
      if (schemaRegex.findFirstMatchIn(url).nonEmpty) ""
      else "http://"

    Future(
      basicRequest
        .get(uri"${urlPrefix + url}")
        .header("user-agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/81.0.4044.138 Safari/537.36")
        .send(backend)
    ).map(r => NodeResponse(r.code.code, r.body.fold(identity, identity)))
  }
}
