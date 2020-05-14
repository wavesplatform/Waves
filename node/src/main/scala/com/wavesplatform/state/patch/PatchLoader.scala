package com.wavesplatform.state.patch

import java.io.File
import java.nio.file.Files

import com.wavesplatform.account.AddressScheme
import com.wavesplatform.utils.ScorexLogging
import play.api.libs.json.{Json, Reads}

object PatchLoader extends ScorexLogging {
  def read[T: Reads](name: AnyRef): T = {
    val path  = getResourcePath(s"patches/$name-${AddressScheme.current.chainId.toChar}.json")
    val bytes = Files.readAllBytes(path.toPath)
    Json.parse(bytes).as[T]
  }

  private[this] def getResourcePath(path: String) = {
    val loader = Thread.currentThread.getContextClassLoader
    val url    = loader.getResource(path)
    new File(url.toURI)
  }
}
