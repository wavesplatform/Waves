package com.wavesplatform.utils
import com.fasterxml.jackson.databind.ObjectMapper
import com.wavesplatform.common.utils.Base64
import com.wavesplatform.lang.API
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3

import java.net.URI
import java.net.http.HttpResponse.BodyHandlers
import java.net.http.{HttpClient, HttpRequest}
import scala.jdk.CollectionConverters.*

object ExternalSourcesTest extends App {
  private val client    = HttpClient.newHttpClient()
  private val reader    = new ObjectMapper()
  private val estimator = ScriptEstimatorV3(true, false)

  private def compileOnGithub(dirUrl: String): Unit = {
    val response = client.send(HttpRequest.newBuilder().uri(new URI(dirUrl)).build(), BodyHandlers.ofString)
    reader
      .readTree(response.body())
      .elements()
      .asScala
      .foreach { entry =>
        val entryType = entry.get("type").asText()
        if (entryType == "file") {
          val script  = client.send(HttpRequest.newBuilder().uri(new URI(entry.get("_links").get("git").asText())).build(), BodyHandlers.ofString)
          val content = reader.readTree(script.body()).get("content").asText()
          val result  = API.compile(new String(Base64.decode(content.replace("\n", ""))), estimator).isRight
          if (result)
            println(s"successfully compiled ${entry.get("path")}")
        } else if (entryType == "dir") {
          compileOnGithub(entry.get("_links").get("self").asText())
        }
      }
  }

  compileOnGithub("https://api.github.com/repos/waves-exchange/neutrino-contract/contents/script")
}
