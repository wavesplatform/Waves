package com.wavesplatform.utils
import com.fasterxml.jackson.databind.ObjectMapper
import com.wavesplatform.common.utils.Base64
import com.wavesplatform.lang.API
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3

import java.net.http.HttpResponse.BodyHandlers
import java.net.http.{HttpClient, HttpRequest}
import java.net.{URI, URLEncoder}
import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters.*

object ExternalSourcesTest extends App {
  private val client    = HttpClient.newHttpClient()
  private val reader    = new ObjectMapper()
  private val estimator = ScriptEstimatorV3(true, false)

  private def compileFromGithub(dirUrl: String): Unit = {
    val directoryTree = client.send(githubRequest(dirUrl), BodyHandlers.ofString).body()
    reader
      .readTree(directoryTree)
      .elements()
      .asScala
      .foreach { entry =>
        entry.get("type").asText() match {
          case "dir" =>
            compileFromGithub(entry.get("_links").get("self").asText())
          case "file" if entry.get("path").asText().endsWith(".ride") =>
            val request  = githubRequest(entry.get("_links").get("git").asText())
            val response = client.send(request, BodyHandlers.ofString).body()
            val base64   = reader.readTree(response).get("content").asText()
            val script   = new String(Base64.decode(base64.replace("\n", "")))
            API
              .compile(script, estimator)
              .fold(
                error => throw new RuntimeException(s"$error on ${entry.get("path")} compilation"),
                _ => println(s"successfully compiled ${entry.get("path")}")
              )
          case _ =>
        }
      }
  }

  private def githubRequest(url: String) =
    HttpRequest
      .newBuilder()
      .uri(new URI(url))
      .setHeader("Authorization", s"Bearer ${System.getenv("DUCKS_GITHUB_TOKEN")}")
      .build()

  private def compileFromGitlab(baseUrl: String, path: String): Unit = {
    val request       = gitlabRequest(baseUrl, path, isTree = true)
    val directoryTree = client.send(request, BodyHandlers.ofString).body()
    reader
      .readTree(directoryTree)
      .elements()
      .asScala
      .foreach { entry =>
        entry.get("type").asText() match {
          case "tree" =>
            compileFromGitlab(baseUrl, entry.get("path").asText())
          case "blob" if entry.get("name").asText().endsWith(".ride") =>
            val request  = gitlabRequest(baseUrl, entry.get("path").asText(), isTree = false)
            val response = client.send(request, BodyHandlers.ofString).body()
            val base64   = reader.readTree(response).get("content").asText()
            val script   = new String(Base64.decode(base64))
            API
              .compile(script, estimator)
              .fold(
                error => throw new RuntimeException(s"$error on ${entry.get("path")} compilation"),
                _ => println(s"successfully compiled ${entry.get("path")}")
              )
          case _ =>
        }
      }
  }

  private def gitlabRequest(baseUrl: String, path: String, isTree: Boolean) = {
    val encoded = URLEncoder.encode(path, StandardCharsets.UTF_8)
    val url     = if (isTree) s"$baseUrl/tree?path=$encoded" else s"$baseUrl/files/$encoded?ref=master"
    HttpRequest
      .newBuilder()
      .uri(URI.create(url))
      .setHeader("PRIVATE-TOKEN", System.getenv("SWOPFI_GITLAB_TOKEN"))
      .build()
  }

  println("Compiling Neutrino contracts:")
  compileFromGithub("https://api.github.com/repos/waves-exchange/neutrino-contract/contents/script")
  println("Compiling WX contracts:")
  compileFromGithub("https://api.github.com/repos/waves-exchange/contracts/contents/ride")
  println("Compiling Ducks contracts:")
  compileFromGithub("https://api.github.com/repos/akharazyan/wavesducks-public/contents/ride")
  println("Compiling Swop.fi contracts:")
  compileFromGitlab("https://gitlabwp.wvservices.com/api/v4/projects/29/repository", "dApps")
}
