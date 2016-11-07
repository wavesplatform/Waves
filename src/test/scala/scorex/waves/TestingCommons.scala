package scorex.waves

import java.util.concurrent.atomic.AtomicInteger
import com.ning.http.client.Response
import com.wavesplatform.{Application, TestNetParams}
import dispatch.{Http, url}
import play.api.libs.json.{JsObject, JsValue, Json}
import scorex.account.AddressScheme
import scorex.transaction.TransactionSettings
import scorex.utils._
import com.wavesplatform.settings.WavesSettings
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import akka.actor.ActorSystem
import scorex.settings.Settings

trait TestingCommons {

  implicit object TestTransactionLayerSettings extends TransactionSettings {
    override val settingsJSON: JsObject = Json.obj()
  }

}

object TestingCommons {
  AddressScheme.current = TestNetParams.addressScheme
  lazy val applications = {
    val apps = List(
      new Application(ActorSystem("test"), new WavesSettings(Settings.readSettingsJson("settings-test.json")) {
        override lazy val chainParams = TestNetParams
        override lazy val walletDirOpt = None
        override lazy val dataDirOpt = None
      })
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

  lazy val counter: AtomicInteger = new AtomicInteger(0)

  def start(): Unit = {
    counter.incrementAndGet
  }

  def stop(): Unit = {
    if (counter.decrementAndGet == 0) {
      Http.shutdown()
      application.shutdown()
    }
  }

  def peerUrl(a: Application = application): String =
    "http://" + a.settings.bindAddress + ":" + a.settings.rpcPort

  def getRequest(us: String, peer: String = peerUrl(application),
                 headers: Map[String, String] = Map.empty): JsValue = {
    val request = Http(url(peer + us).GET <:< headers)
    val response = Await.result(request, 10.seconds)
    Json.parse(response.getResponseBody)
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

  def postRequestWithResponse(us: String,
                              params: Map[String, String] = Map.empty,
                              body: String = "",
                              headers: Map[String, String] = Map("api_key" -> "test"),
                              peer: String = peerUrl(application)): Response = {
    val request = Http(url(peer + us).POST << params <:< headers << body)
    Await.result(request, 5.seconds)
  }
}
