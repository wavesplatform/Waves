package scorex.transaction

import play.api.libs.json.JsObject

trait TransactionSettings {
  val settingsJSON: JsObject

  lazy val dataDirOpt = {
    val res = (settingsJSON \ "datadir").asOpt[String]
    res.foreach(folder => new java.io.File(folder).mkdirs())
    res
  }
  //BLOCKCHAIN
  lazy val maxRollback = 100
}
