package scorex.waves

import dispatch.{Http, url}
import play.api.libs.json.{JsObject, JsValue, Json}
import scorex.transaction.TransactionSettings
import scorex.utils._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

trait TestingCommons {

  implicit object TestTransactionLayerSettings extends TransactionSettings {
    override val settingsJSON: JsObject = Json.obj()
  }

}

object TestingCommons {
  lazy val applications = {
    val apps = List(
      new Application("settings-test.json")
    )
    apps.foreach(_.run())
    apps.foreach { a =>
      if (a.wallet.privateKeyAccounts().isEmpty) a.wallet.generateNewAccounts(3)
      untilTimeout(20.seconds, 1.second) {
        val request = Http(url(peerUrl(a) + "/consensus/algo").GET)
        val response = Await.result(request, 10.seconds)
        val json = Json.parse(response.getResponseBody).as[JsObject]
        assert((json \ "consensus-algo").asOpt[String].isDefined)
      }
    }
    apps
  }

  lazy val application = applications.head

  def peerUrl(a: Application = application): String =
    "http://" + a.settings.bindAddress + ":" + a.settings.rpcPort

  def getRequest(us: String, peer: String = peerUrl(application)): JsValue = {
    val request = Http(url(peer + us).GET)
    val response = Await.result(request, 10.seconds)
    Json.parse(response.getResponseBody)
  }

}
