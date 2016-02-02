package scorex.transaction

import java.io.File

import play.api.libs.json.JsObject

trait TransactionSettings {
  val settingsJSON: JsObject

  lazy val dataDirOpt = {
    val res = (settingsJSON \ "dataDir").asOpt[String]
    res.foreach(folder => require(new File(folder).mkdirs()))
    res
  }

  private val DefaultHistory = "blockchain"
  lazy val history = (settingsJSON \ "history").asOpt[String].getOrElse(DefaultHistory)

  private val DefaultMaxRollback = 100
  lazy val MaxRollback = (settingsJSON \ "max-rollback").asOpt[Int].getOrElse(DefaultMaxRollback)

}
