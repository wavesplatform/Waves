package com.wavesplatform.it

import com.typesafe.config.Config
import org.asynchttpclient.AsyncHttpClient
import play.api.libs.json._

import scala.concurrent.Future
import scala.compat.java8.FutureConverters._
import scala.concurrent.ExecutionContext.Implicits.global


class Node(config: Config, nodeInfo: NodeInfo, client: AsyncHttpClient) {
  import Node._

  def prepareGet(path: String) = client.prepareGet(s"http://localhost:${nodeInfo.hostRestApiPort}$path")
  def connectedPeers: Future[Seq[Peer]] = prepareGet("/peers/connected")
    .execute().toCompletableFuture.toScala.map { r =>
      (Json.parse(r.getResponseBody) \ "peers").as[Seq[Peer]]
    }
}

object Node {
  private[Node] case class Peer(address: String, declaredAddress: String, peerName: String)
  implicit val peerFormat: Format[Peer] = Json.format
}