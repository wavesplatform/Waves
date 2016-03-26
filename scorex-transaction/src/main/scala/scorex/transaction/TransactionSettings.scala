package scorex.transaction

import play.api.libs.json.JsObject

trait TransactionSettings {
  val settingsJSON: JsObject

  private val DefaultHistory = "blockchain"
  lazy val history = (settingsJSON \ "history").asOpt[String].getOrElse(DefaultHistory)

  private val DefaultMaxRollback = 100
  lazy val MaxRollback = (settingsJSON \ "max-rollback").asOpt[Int].getOrElse(DefaultMaxRollback)

}
