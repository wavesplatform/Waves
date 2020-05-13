package com.wavesplatform.state.patch

import java.io.File
import java.nio.file.{Files, Paths}

import com.wavesplatform.account.AddressScheme
import com.wavesplatform.utils.ScorexLogging
import play.api.libs.json.{Json, Reads, Writes}

object PatchLoader extends ScorexLogging {
  final case class Patch(name: String, chainId: Byte, height: Int) {
    private[PatchLoader] def fileName: String = s"$name-${chainId.toChar}-patch.json"
  }

  lazy val patches: Seq[Patch] = {
    def getResourceFolderFiles(folder: String) = {
      val path = getResourcePath(folder)
      new File(path).listFiles.toVector.map(_.getName)
    }

    val regex = "(\\w+)-(\\w)-(\\d+)-patch.json".r
    getResourceFolderFiles("patches").collect {
      case regex(name, chainId, height) =>
        Patch(name, chainId.head.toByte, height.toInt)
    }.filter(_.chainId == AddressScheme.current.chainId)
  }

  def isDiffPatch(name: String): Boolean =
    name != "DisableHijackedAliases"

  def read[T: Reads](patch: Patch): T = {
    val path  = getResourcePath(s"patches/${patch.fileName}")
    val bytes = Files.readAllBytes(Paths.get(path))
    Json.parse(bytes).as[T]
  }

  def write[T: Writes](name: String, height: Int, value: T): Unit = {
    val chainId = AddressScheme.current.chainId.toChar
    val path    = Paths.get(sys.props("user.home"), s"$name-$chainId-$height-patch.json")
    val json    = Json.prettyPrint(Json.toJson(value))
    log.info(s"Patch $name at height $height saved to $path")
    Files.write(path, json.getBytes("UTF-8"))
  }

  private[this] def getResourcePath(path: String) = {
    val loader = Thread.currentThread.getContextClassLoader
    val url    = loader.getResource(path)
    url.getPath
  }
}
