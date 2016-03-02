package scorex.perma.settings

import java.io.File

import play.api.libs.json.JsObject
import scorex.crypto.encode.Base58

trait PermaSettings {
  val settingsJSON: JsObject

  lazy val rootHash: Array[Byte] = Base58.decode("13uSUANWHG7PaCac7i9QKDZriUNKXCi84UkS3ijGYTm1").get

  lazy val isTrustedDealer = (settingsJSON \ "perma" \ "isTrustedDealer").asOpt[Boolean].getOrElse(false)

  lazy val treeDir = {
    val dir = (settingsJSON \ "perma" \ "treeDir").as[String]
    new File(dir).mkdirs()
    dir
  }

  lazy val authDataStorage = treeDir + (settingsJSON \ "perma" \ "authDataStorage").as[String]
}
