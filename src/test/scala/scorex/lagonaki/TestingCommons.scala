package scorex.lagonaki

import play.api.libs.json.{JsObject, Json}
import scorex.lagonaki.server.LagonakiApplication
import scorex.transaction.TransactionSettings

trait TestingCommons {

  implicit object TestTransactionLayerSettings extends TransactionSettings {
    override val settingsJSON: JsObject = Json.obj()
  }

}

object TestingCommons {
  val applications = List(
    new LagonakiApplication("settings-test.json"),
    new LagonakiApplication("settings-local1.json"),
    new LagonakiApplication("settings-local2.json")
  )

  val application = applications.head

  def peerUrl(a: LagonakiApplication = application): String =
    "http://" + a.settings.bindAddress + ":" + a.settings.rpcPort

}
