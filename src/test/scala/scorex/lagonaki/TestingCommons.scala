package scorex.lagonaki

import java.util.concurrent.locks.ReentrantLock

import akka.pattern.ask
import akka.util.Timeout
import com.ning.http.client.Response
import dispatch.{Http, url}
import play.api.libs.json.{JsObject, JsValue, Json}
import scorex.api.http.ApiKeyNotValid
import scorex.app.Application.GetBlockGenerationStatus
import scorex.consensus.mining.BlockGeneratorController.{Idle, StartGeneration, StopGeneration}
import scorex.lagonaki.server.LagonakiApplication
import scorex.transaction.TransactionSettings
import scorex.utils._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Random

trait TestingCommons {

  implicit object TestTransactionLayerSettings extends TransactionSettings {
    override val settingsJSON: JsObject = Json.obj()
  }

  lazy val application = TestingCommons.application

  def randomFrom[T](seq: Seq[T]): T = {
    require(seq.nonEmpty)
    seq(Random.nextInt(seq.length))
  }

  def profile[R](block: => R): Long = {
    val start = System.currentTimeMillis()
    block
    System.currentTimeMillis() - start
  }
}

object TestingCommons {

  implicit val timeout = Timeout(1.second)

  val lock = new ReentrantLock(true)

  lazy val applications = {
    val apps = List(
      new LagonakiApplication("settings-test.json"),
      new LagonakiApplication("settings-local1.json"),
      new LagonakiApplication("settings-local2.json")
    )
    apps.foreach(_.run())
    apps.foreach { a =>
      if (a.wallet.privateKeyAccounts().isEmpty) a.wallet.generateNewAccounts(3)
      untilTimeout(20.seconds, 1.second) {
        val request = Http(url(peerUrl(a) + "/consensus/algo").GET)
        val response = Await.result(request, 10.seconds)
        val json = Json.parse(response.getResponseBody).as[JsObject]
        assert((json \ "consensusAlgo").asOpt[String].isDefined)
      }
    }
    apps
  }

  lazy val application = applications.head

  def startGeneration(nodes: Seq[LagonakiApplication]): Unit = {
    nodes.foreach(_.blockGenerator ! StartGeneration)
  }

  def stopGeneration(nodes: Seq[LagonakiApplication]): Unit = {
    nodes.foreach(_.blockGenerator ! StopGeneration)
    untilTimeout(5.seconds) {
      nodes.foreach { p =>
        require(Await.result(p.blockGenerator ? GetBlockGenerationStatus, timeout.duration) == Idle.name)
      }
    }
  }

  def peerUrl(a: LagonakiApplication = application): String =
    "http://" + a.settings.bindAddress + ":" + a.settings.rpcPort

  def forgeSignature(signature: Array[Byte]): Array[Byte] = {
    val modifier: BigInt = BigInt("7237005577332262213973186563042994240857116359379907606001950938285454250989")
    signature.take(32) ++ (BigInt(signature.takeRight(32).reverse) + modifier).toByteArray.reverse
  }

  def postRequest(us: String,
                  params: Map[String, String] = Map.empty,
                  body: String = "",
                  headers: Map[String, String] = Map("api_key" -> "test"),
                  peer: String = peerUrl(application)): JsValue = {
    val request = Http(url(peer + us).POST << params <:< headers << body)
    val response = Await.result(request, 5.seconds)
    Json.parse(response.getResponseBody)
  }

  sealed trait RequestType {
    def incorrectApiKeyTest(path: String): Unit = {
      Seq(Map[String, String](), Map("api_key" -> "wrong key")) foreach { h =>
        val resp = request(path, headers = h).toString()
        assert(resp == ApiKeyNotValid.json.toString(), s"$resp == ${ApiKeyNotValid.json.toString()} is false")
      }
    }

    def request(us: String,
                params: Map[String, String] = Map.empty,
                body: String = "",
                headers: Map[String, String] = Map("api_key" -> "test"),
                peer: String = peerUrl(application)): JsValue

    def requestRaw(us: String,
                params: Map[String, String] = Map.empty,
                body: String = "",
                headers: Map[String, String] = Map("api_key" -> "test"),
                peer: String = peerUrl(application)): Response
  }

  case object GET extends RequestType {
    def request(us: String,
                params: Map[String, String] = Map.empty,
                body: String = "",
                headers: Map[String, String] = Map.empty,
                peer: String = peerUrl(application)): JsValue = {
      val request = Http(url(peer + us).GET <:< headers)
      val response: Response = Await.result(request, 5.seconds)
      Json.parse(response.getResponseBody)
    }

    override def requestRaw(us: String, params: Map[String, String], body: String, headers: Map[String, String], peer: String): Response = {
      val request = Http(url(peer + us).GET <:< headers)
      Await.result(request, 5.seconds)
    }
  }

  case object POST extends RequestType {
    def request(us: String,
                params: Map[String, String] = Map.empty,
                body: String = "",
                headers: Map[String, String] = Map("api_key" -> "test"),
                peer: String = peerUrl(application)): JsValue = {
      val request = Http(url(peer + us).POST << params <:< headers << body)
      val response = Await.result(request, 5.seconds)
      Json.parse(response.getResponseBody)
    }

    override def requestRaw(us: String, params: Map[String, String], body: String, headers: Map[String, String], peer: String): Response = {
      val request = Http(url(peer + us).POST << params <:< headers << body)
      Await.result(request, 5.seconds)
    }
  }

  case object DELETE extends RequestType {
    def request(us: String,
                params: Map[String, String] = Map.empty,
                body: String = "",
                headers: Map[String, String] = Map("api_key" -> "test"),
                peer: String = peerUrl(application)): JsValue = {
      val request = Http(url(peer + us).DELETE << params <:< headers << body)
      val response = Await.result(request, 5.seconds)
      Json.parse(response.getResponseBody)
    }

    override def requestRaw(us: String, params: Map[String, String], body: String, headers: Map[String, String], peer: String): Response = {
      val request = Http(url(peer + us).DELETE << params <:< headers << body)
      Await.result(request, 5.seconds)
    }
  }
}
