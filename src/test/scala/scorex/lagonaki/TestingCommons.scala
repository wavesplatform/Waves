package scorex.lagonaki

import dispatch.{Http, url}
import play.api.libs.json.{JsObject, JsValue, Json}
import scorex.api.http.ApiKeyNotValid
import scorex.lagonaki.TestingCommons._
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

  implicit val consensusModule = application.consensusModule
  implicit val transactionModule = application.transactionModule

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
  lazy val applications = initialize(Seq(
    "settings-test.json", "settings-local1.json", "settings-local2.json"))

  lazy val application = applications.head

  def initialize(settingsFileNames: Seq[String]): Seq[LagonakiApplication] = {
    val apps = settingsFileNames.map(new LagonakiApplication(_))

    apps.foreach { app =>
      app.run()
      Thread sleep 500
    }

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

  def peerUrl(a: LagonakiApplication): String =
    "http://" + a.settings.bindAddress + ":" + a.settings.rpcPort

  def postRequest(us: String,
                  params: Map[String, String] = Map.empty,
                  body: String = "",
                  headers: Map[String, String] = Map("api_key" -> "test"),
                  peer: String = peerUrl(application)): JsValue = {
    val request = Http(url(peer + us).POST << params <:< headers << body)
    val response = Await.result(request, 5.seconds)
    Json.parse(response.getResponseBody)
  }


  @deprecated("Replace with GET.request", "1.2.6")
  def getRequest(us: String, peer: String = peerUrl(application)): JsValue = GET.request(us = us, peer = peer)

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
  }

  case object GET extends RequestType {
    def request(us: String,
                params: Map[String, String] = Map.empty,
                body: String = "",
                headers: Map[String, String] = Map.empty,
                peer: String = peerUrl(application)): JsValue = {
      val request = Http(url(peer + us).GET <:< headers)
      val response = Await.result(request, 5.seconds)
      Json.parse(response.getResponseBody)
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

  }

}
