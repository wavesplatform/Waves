package com.wavesplatform.state.patch

import java.io.File
import java.nio.file.{Files, Paths}

import com.wavesplatform.account.AddressScheme
import com.wavesplatform.utils.ScorexLogging
import play.api.libs.json.{Json, Reads}

object PatchLoader extends ScorexLogging {
  final case class Patch(name: String, chainId: Char, height: Int) {
    private[PatchLoader] def fileName: String = s"$name-${chainId.toChar}-patch.json"
  }

  lazy val patches: Seq[Patch] = loadPatches()

  def isDiffPatch(name: String): Boolean =
    name != "DisableHijackedAliases"

  def read[T: Reads](patch: Patch): T = {
    assert(patch.chainId == AddressScheme.current.chainId)
    val path  = getResourcePath(s"patches/${patch.fileName}")
    val bytes = Files.readAllBytes(Paths.get(path))
    Json.parse(bytes).as[T]
  }

  private[this] def loadPatches() = {
    def getResourceFolderFiles(folder: String) = {
      val path = getResourcePath(folder)
      new File(path).listFiles.toVector.map(_.getName)
    }

    val regex = "(\\w+)-(\\w)-(\\d+)-patch.json".r
    getResourceFolderFiles("patches")
      .collect {
        case regex(name, chainId, height) =>
          Patch(name, chainId.head, height.toInt)
      }
      .filter(_.chainId.toByte == AddressScheme.current.chainId)
  }

  private[this] def getResourcePath(path: String) = {
    val loader = Thread.currentThread.getContextClassLoader
    val url    = loader.getResource(path)
    url.getPath
  }
}
