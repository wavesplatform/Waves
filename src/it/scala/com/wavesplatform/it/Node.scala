package com.wavesplatform.it

import com.typesafe.config.Config
import org.asynchttpclient.{AsyncHttpClient, BoundRequestBuilder, Response}
import play.api.libs.json._

import scala.compat.java8.FutureConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


class Node(config: Config, nodeInfo: NodeInfo, client: AsyncHttpClient) {
  import Node._

  val privateKey = config.getString("private-key")
  val publicKey = config.getString("public-key")
  val address = config.getString("address")

  def get(path: String, f: BoundRequestBuilder => BoundRequestBuilder = identity): Future[Response] =
    f(client.prepareGet(s"http://localhost:${nodeInfo.hostRestApiPort}$path"))
      .execute()
      .toCompletableFuture
      .toScala

  def connectedPeers: Future[Seq[Peer]] = get("/peers/connected").map { r =>
    (Json.parse(r.getResponseBody) \ "peers").as[Seq[Peer]]
  }

  def status: Future[Status] = get("/node/status").map { r =>
    Json.parse(r.getResponseBody).as[Status]
  }
}

object Node {
  case class Status(blockGeneratorStatus: String, historySynchronizationStatus: String)
  implicit val statusFormat: Format[Status] = Json.format

  case class Peer(address: String, declaredAddress: String, peerName: String)
  implicit val peerFormat: Format[Peer] = Json.format
}