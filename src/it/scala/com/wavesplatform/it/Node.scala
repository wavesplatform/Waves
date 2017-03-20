package com.wavesplatform.it

import com.typesafe.config.Config
import io.netty.util.{HashedWheelTimer, Timeout}
import org.asynchttpclient._
import org.asynchttpclient.Dsl.{get => _get, post => _post}
import play.api.libs.json._
import play.api.libs.json.Json._
import scorex.api.http.assets.TransferRequest

import scala.compat.java8.FutureConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}
import scala.concurrent.duration._


class Node(config: Config, nodeInfo: NodeInfo, client: AsyncHttpClient, timer: HashedWheelTimer) {
  import Node._

  val privateKey = config.getString("private-key")
  val publicKey = config.getString("public-key")
  val address = config.getString("address")

  private def retrying(r: Request, interval: FiniteDuration = 200.millis): Future[Response] = {
    val p = Promise[Response]

    def execute(t: Timeout): Unit = client.executeRequest(r).toCompletableFuture.toScala.onComplete {
      case s@Success(_) => p.complete(s)
      case Failure(_) => timer.newTimeout(execute, interval.toMillis, MILLISECONDS)
    }

    execute(null)

    p.future
  }

  def get(path: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] =
    retrying(f(_get(s"http://localhost:${nodeInfo.hostRestApiPort}$path")).build())

  def post(path: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] =
    retrying(f(
      _post(s"http://localhost:${nodeInfo.hostRestApiPort}$path").setHeader("api_key", "integration-test-rest-api")
    ).build())

  def post[A: Writes](path: String, body: A): Future[Response] =
    post(path, (rb: RequestBuilder) => rb.setHeader("Content-type", "application/json").setBody(stringify(toJson(body))))

  def connectedPeers: Future[Seq[Peer]] = get("/peers/connected").map { r =>
    (Json.parse(r.getResponseBody) \ "peers").as[Seq[Peer]]
  }

  def height: Future[Long] = get("/blocks/height").as[JsValue].map(v => (v \ "height").as[Long])
  def blockAt(height: Long) = get(s"/blocks/at/$height").as[Block]
  def status: Future[Status] = get("/node/status").as[Status]
  def balance(address: String): Future[Balance] = get(s"/addresses/balance/$address").as[Balance]
  def transfer(sourceAddress: String, recipient: String, amount: Long, fee: Long): Future[String] =
    post("/assets/transfer", TransferRequest(None, None, amount, fee, sourceAddress, None, recipient))
      .map(_.getResponseBody)
}

object Node {
  case class Status(blockGeneratorStatus: String, historySynchronizationStatus: String)
  implicit val statusFormat: Format[Status] = Json.format

  case class Peer(address: String, declaredAddress: String, peerName: String)
  implicit val peerFormat: Format[Peer] = Json.format

  case class Balance(address: String, confirmations: Int, balance: Long)
  implicit val balanceFormat: Format[Balance] = Json.format

  case class Block(signature: String)
  implicit val blockFormat: Format[Block] = Json.format

  implicit class ResponseFutureExt(val f: Future[Response]) extends AnyVal {
    def as[A: Format](implicit ec: ExecutionContext): Future[A] = f.map(r => Json.parse(r.getResponseBody).as[A])(ec)
  }
}