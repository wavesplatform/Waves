package scorex.transaction

import java.io.File

import play.api.libs.json.JsObject

trait TransactionSettings {
  val settingsJSON: JsObject

  lazy val dataDirOpt = {
    val res = (settingsJSON \ "dataDir").asOpt[String]
    res.foreach(folder => new File(folder).mkdirs())
    require(res.isEmpty || new File(res.get).exists())
    res
  }

  private val DefaultHistory = "blockchain"
  lazy val history = (settingsJSON \ "history").asOpt[String].getOrElse(DefaultHistory)

  private val DefaultMaxRollback = 100
  lazy val MaxRollback = (settingsJSON \ "max-rollback").asOpt[Int].getOrElse(DefaultMaxRollback)

}
