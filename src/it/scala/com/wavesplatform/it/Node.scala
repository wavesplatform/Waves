package com.wavesplatform.it

import com.typesafe.config.Config
import org.asynchttpclient.{AsyncHttpClient, BoundRequestBuilder, Response}
import play.api.libs.json._

import scala.compat.java8.FutureConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}


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

  def post(path: String, f: BoundRequestBuilder => BoundRequestBuilder = identity): Future[Response] =
    f(client.preparePost(s"http://localhost:${nodeInfo.hostRestApiPort}$path"))
      .execute()
      .toCompletableFuture
      .toScala

  def connectedPeers: Future[Seq[Peer]] = get("/peers/connected").map { r =>
    (Json.parse(r.getResponseBody) \ "peers").as[Seq[Peer]]
  }

  def status: Future[Status] = get("/node/status").as[Status]
  def balance(address: String): Future[Balance] = get(s"/addresses/balance/$address").as[Balance]
  def transfer(sourceAddress: String, recipient: String, amount: Long, fee: Long): Future[String] =
    post(
      "/assets/transfer",
      _.setHeader("api_key", "integration-test-rest-api")
        .setHeader("Content-type", "application/json")
        .setBody(Json.stringify(Json.obj(
        "assetId" -> JsNull,
        "feeAssetId" -> JsNull,
        "amount" -> amount,
        "fee" -> fee,
        "sender" -> sourceAddress,
        "recipient" -> recipient,
        "attachment" -> JsNull)))).map(_.getResponseBody)
}

object Node {
  case class Status(blockGeneratorStatus: String, historySynchronizationStatus: String)
  implicit val statusFormat: Format[Status] = Json.format

  case class Peer(address: String, declaredAddress: String, peerName: String)
  implicit val peerFormat: Format[Peer] = Json.format

  case class Balance(address: String, confirmations: Int, balance: Long)
  implicit val balanceFormat: Format[Balance] = Json.format

  implicit class ResponseFutureExt(val f: Future[Response]) extends AnyVal {
    def as[A: Format](implicit ec: ExecutionContext): Future[A] = f.map(r => Json.parse(r.getResponseBody).as[A])(ec)
  }
}