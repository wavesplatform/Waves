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

  private def compileFromGithub(dirUrl: String): Unit = {
    val request  = HttpRequest.newBuilder().uri(new URI(dirUrl)).build()
    val response = client.send(request, BodyHandlers.ofString)
    reader
      .readTree(response.body())
      .elements()
      .asScala
      .foreach { entry =>
        entry.get("type").asText() match {
          case "file" =>
            val scriptUrl = new URI(entry.get("_links").get("git").asText())
            val request   = HttpRequest.newBuilder().uri(scriptUrl).build()
            val response  = client.send(request, BodyHandlers.ofString)
            val base64    = reader.readTree(response.body()).get("content").asText()
            val name      = entry.get("path")
            val script    = new String(Base64.decode(base64.replace("\n", "")))
            API
              .compile(script, estimator)
              .fold(
                error => throw new RuntimeException(s"$error on $name compilation"),
                _ => println(s"successfully compiled $name")
              )
          case "dir" =>
            compileFromGithub(entry.get("_links").get("self").asText())
        }
      }
  }

  println("Compiling Neutrino contracts:")
  compileFromGithub("https://api.github.com/repos/waves-exchange/neutrino-contract/contents/script")
  println("Compiling WX contracts:")
  compileFromGithub("https://api.github.com/repos/waves-exchange/contracts/contents/ride")
}
