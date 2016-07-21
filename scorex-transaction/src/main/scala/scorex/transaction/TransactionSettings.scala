package scorex.transaction

import play.api.libs.json.JsObject
import scala.concurrent.duration._

trait TransactionSettings {
  val settingsJSON: JsObject

  private val DefaultHistory = "blockchain"
  lazy val history = (settingsJSON \ "history").asOpt[String].getOrElse(DefaultHistory)

  private val DefaultUtxRebroadcastInterval: FiniteDuration = 30.seconds
  lazy val utxRebroadcastInterval: FiniteDuration = (settingsJSON \ "utxRebroadcastInterval").asOpt[Int]
    .map(x => x.seconds).getOrElse(DefaultUtxRebroadcastInterval)
}
